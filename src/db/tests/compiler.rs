use std::{collections::{BTreeMap, BTreeSet}, rc::Rc};

use crate::{Db, DbHandle, Run, Value};

/// Test a somewhat more complex case with nested computations
/// There is a lot of cloning required to create these keys. This can be
/// improved in the future with interning. For now we manually wrap arguments
/// in `Rc` to reduce the cost a bit. This still requires cloning when - e.g.
/// the argument is mutated such as with the environment parameters.
#[derive(Debug, PartialEq, Eq, Hash)]
enum Compiler {
    /// Input: String
    Input,

    /// fn parse() -> Result<Ast, Error>
    ///   depends on: Input
    Parse,

    /// fn check(ast: Rc<Ast>, env: Rc<CheckEnv>) -> Result<(), Error>
    ///   depends on: Check(subtree, _)
    Check(Rc<Ast>, Rc<CheckEnv>),

    /// fn execute(ast: Rc<Ast>, env: Rc<Env>) -> Result<i64, Error>
    ///   depends on: Execute(subtree, _)
    Execute(Rc<Ast>, Rc<ExecEnv>),

    /// fn execute_all() -> Result<i64, Error>
    ///   depends on: Check(..), Execute(..)
    ExecuteAll,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
enum Error {
    IncorrectArgumentCount,
    UnterminatedLParen(Ast),
    InvalidOperation(String),
    InvalidIntegerLiteral(String),
    LetVarIsNotAnIdent(Ast),
    InputEmptyOrUnparsedOutput(String),

    NameNotDefined(String),
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
enum Ast {
    Var { name: String },
    Int(i64),
    Add(Rc<Ast>, Rc<Ast>),
    Let { name: String, rhs: Rc<Ast>, body: Rc<Ast> },
}

type CheckEnv = BTreeSet<String>;
type ExecEnv = BTreeMap<String, i64>;

impl Run for Compiler {
    fn run(&self, db: &mut DbHandle<Self>) -> Value {
        use Compiler::*;
        match self {
            Input => panic!("Initial input not set!"),
            Parse => Value::new(parse_program(db)),
            Check(ast, env) => Value::new(check(ast, env, db)),
            Execute(ast, env) => Value::new(execute(ast, env, db)),
            ExecuteAll => Value::new(execute_all(db)),
        }
    }
}

/// Parse an s-expr:
///
/// program: expr
/// expr: variable | integer | add | let
/// variable: [A-Za-z_]+
/// integer: -?[0-9]
/// add: '(' '+' expr expr ')'
/// let: '(' 'let' variable expr expr ')'
fn parse_program(db: &mut DbHandle<Compiler>) -> Result<Ast, Error> {
    let program: &String = db.get(Compiler::Input);
    let (ast, rest) = parse_value(program)?;

    if !rest.trim().is_empty() {
        Err(Error::InputEmptyOrUnparsedOutput(rest.to_string()))
    } else {
        Ok(ast)
    }
}

/// Given an s-expr call `(f a (b c) d)` return `vec!["f", "a", "(b c)", "d"]`
/// Errors if the parenthesis are mismatched
fn parse_value(mut text: &str) -> Result<(Ast, &str), Error> {
    text = text.trim();

    let Some(first_char) = text.chars().next() else {
        return Err(Error::InputEmptyOrUnparsedOutput(text.to_string()));
    };

    let ast = match first_char {
        '(' => {
            text = &text[1..];
            let mut args = Vec::new();

            let (operation, new_text) = parse_word(text);
            text = new_text;

            while let Ok((ast, rest)) = parse_value(text) {
                args.push(ast);
                text = rest;
            }

            let ast = match operation {
                "+" => {
                    if args.len() != 2 {
                        return Err(Error::IncorrectArgumentCount);
                    }
                    let rhs = args.pop().unwrap();
                    let lhs = args.pop().unwrap();
                    Ast::Add(Rc::new(lhs), Rc::new(rhs))
                }
                "let" => {
                    if args.len() != 3 {
                        return Err(Error::IncorrectArgumentCount);
                    }
                    let body = Rc::new(args.pop().unwrap());
                    let rhs = Rc::new(args.pop().unwrap());

                    let let_var = args.pop().unwrap();
                    let Ast::Var { name } = let_var else {
                        return Err(Error::LetVarIsNotAnIdent(let_var));
                    };
                    Ast::Let { name, rhs, body }
                }
                other => return Err(Error::InvalidOperation(other.to_string())),
            };

            text = text.trim();
            if text.chars().next() != Some(')') {
                return Err(Error::UnterminatedLParen(ast))
            } else {
                text = &text[1..];
                ast
            }
        }
        s if s.is_ascii_alphabetic() => {
            let (word, rest) = parse_word(text);
            text = rest;
            Ast::Var { name: word.to_string() }
        },
        s if s.is_numeric() => {
            let (word, rest) = parse_word(text);
            text = rest;
            let int = word.parse::<i64>().map_err(|_| Error::InvalidIntegerLiteral(word.to_string()))?;
            Ast::Int(int)
        },
        _ => return Err(Error::InputEmptyOrUnparsedOutput(text.to_string())),
    };

    Ok((ast, text))
}

/// Returns (word, rest of input)
fn parse_word(text: &str) -> (&str, &str) {
    let end = next_whitespace_or_rparen_index(text);
    if end == text.len() {
        (&text[..end], "")
    } else {
        (&text[..end], &text[end..])
    }
}

/// Returns the index of the next whitespace character or the next ')'.
/// Returns the length of the string if neither are found.
fn next_whitespace_or_rparen_index(text: &str) -> usize {
    for (i, char) in text.char_indices() {
        if char.is_whitespace() || char == ')' {
            return i;
        }
    }
    text.len()
}

/// Ensure all variables are defined or return an Error
fn check(ast: &Ast, env: &Rc<CheckEnv>, db: &mut DbHandle<Compiler>) -> Result<(), Error> {
    match ast {
        Ast::Var { name } => {
            if env.contains(name) {
                Ok(())
            } else {
                Err(Error::NameNotDefined(name.clone()))
            }
        },
        Ast::Int(_) => Ok(()),
        Ast::Add(lhs, rhs) => {
            query_check(lhs.clone(), env.clone(), db)?;
            query_check(rhs.clone(), env.clone(), db)
        },
        Ast::Let { name, rhs, body } => {
            query_check(rhs.clone(), env.clone(), db)?;

            let mut new_env = env.as_ref().clone();
            new_env.insert(name.clone());
            query_check(body.clone(), Rc::new(new_env), db)
        },
    }
}

fn query_check(ast: Rc<Ast>, env: Rc<CheckEnv>, db: &mut DbHandle<Compiler>) -> Result<(), Error> {
    Clone::clone(db.get(Compiler::Check(ast, env)))
}

fn execute(ast: &Rc<Ast>, env: &Rc<ExecEnv>, db: &mut DbHandle<Compiler>) -> Result<i64, Error> {
    match ast.as_ref() {
        Ast::Var { name } => {
            // Assume `check` has already been run and thus that all names are defined
            Ok(env[name])
        },
        Ast::Int(x) => Ok(*x),
        Ast::Add(lhs, rhs) => {
            let lhs = query_execute(lhs.clone(), env.clone(), db)?;
            let rhs = query_execute(rhs.clone(), env.clone(), db)?;
            Ok(lhs + rhs)
        },
        Ast::Let { name, rhs, body } => {
            let rhs = query_execute(rhs.clone(), env.clone(), db)?;

            let mut new_env = env.as_ref().clone();
            new_env.insert(name.clone(), rhs);
            query_execute(body.clone(), Rc::new(new_env), db)
        },
    }
}

/// Can't call this with either a `Db` or `DbHandle` - probably need a trait
fn query_execute(ast: Rc<Ast>, env: Rc<ExecEnv>, db: &mut DbHandle<Compiler>) -> Result<i64, Error> {
    Clone::clone(db.get(Compiler::Execute(ast, env)))
}

/// Without a separate execute_all rule, `execute` would need to call `check`
/// recursively for each sub-tree - at every level of recursion. While in practice the result would
/// be cached which would prevent exponential slowdowns, checking if the dependency is up-to-date
/// at each recursive step is still wasteful. So a separate rule is used here to run the entire
/// check pass, followed by the entire execute pass which resembles how a typical compiler is
/// structured.
fn execute_all(db: &mut DbHandle<Compiler>) -> Result<i64, Error> {
    let ast = db.get::<Result<Ast, Error>>(Compiler::Parse).clone()?;
    let ast = Rc::new(ast);

    db.get::<Result<(), Error>>(Compiler::Check(ast.clone(), Rc::new(CheckEnv::new()))).clone()?;
    Clone::clone(db.get(Compiler::Execute(ast.clone(), Rc::new(ExecEnv::new()))))
}

fn set_input(db: &mut Db<Compiler>, source_program: &str) {
    db.update_input(Compiler::Input, Value::new(source_program.to_string()));
}

/// Helper method to query `ExecuteAll` with the correct output type
fn get_execute_all(db: &mut Db<Compiler>) -> Result<i64, Error> {
    Clone::clone(db.get(Compiler::ExecuteAll))
}

/// Helper method to query `Parse` with the correct output type
fn get_parsed(db: &mut Db<Compiler>) -> Result<Ast, Error> {
    Clone::clone(db.get(Compiler::Parse))
}

#[test]
fn basic_programs() {
    let mut db = Db::<Compiler>::new();

    set_input(&mut db, "42");
    let result = get_execute_all(&mut db);
    assert_eq!(result, Ok(42));

    set_input(&mut db, "(+ 42 58)");
    let result = get_execute_all(&mut db);
    assert_eq!(result, Ok(100));

    set_input(&mut db, "(let foo 42 (+ 58 foo))");
    let result = get_execute_all(&mut db);
    assert_eq!(result, Ok(100));

    set_input(&mut db, "(let foo 42 (+ 58 foo)) foo");
    let result = get_execute_all(&mut db);
    assert_eq!(result, Err(Error::InputEmptyOrUnparsedOutput(" foo".to_string())));

    set_input(&mut db, "(let foo 42 (+ foo bar))");
    let result = get_execute_all(&mut db);
    assert_eq!(result, Err(Error::NameNotDefined("bar".to_string())));

    set_input(&mut db, "(let foo 42 (let bar 8 (+ foo bar)))");
    let result = get_execute_all(&mut db);
    assert_eq!(result, Ok(50));
}

#[test]
fn cached() {
    let mut db = Db::<Compiler>::new();

    set_input(&mut db, "(+ 42 58)");
    let result = get_execute_all(&mut db);
    assert_eq!(result, Ok(100));

    // Update the input, aiming to re-use a previous computation
    set_input(&mut db, "42");
    let ast = get_parsed(&mut db).unwrap();
    assert_eq!(ast, Ast::Int(42));

    eprintln!("Checking if check is stale:");

    // Although the input has changed, we cache each intermediate result and shouldn't
    // need to re-run execute (but execute_all will re-run to produce an Ast that we
    // can check is cached).
    //
    // Note that since the execution environment is cached as well if that had changed
    // then we would still need to re-run this. This is desired for Asts containing Var
    // nodes but not desired for this simple Int node. If we wanted to, we could fix this
    // by adding new rules for ExecuteInt and CheckInt which don't require an environment.
    let ast = Rc::new(ast);
    assert!(!db.is_stale(&Compiler::Check(ast.clone(), Default::default())));
    assert!(!db.is_stale(&Compiler::Execute(ast, Default::default())));
}
