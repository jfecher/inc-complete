use std::{
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};

use inc_complete::{Computation, Db, DbHandle, define_input, define_intermediate};

/// Test a somewhat more complex case with nested computations
/// There is a lot of cloning required to create these keys. This can be
/// improved in the future with interning. For now we manually wrap arguments
/// in `Rc` to reduce the cost a bit. This still requires cloning when - e.g.
/// the argument is mutated such as with the environment parameters.
type Compiler = (Input, Parse, Check, Execute, ExecuteAll);

// Input: String
define_input!(Input, get_input, String);

// fn parse(db) -> Result<Ast, Error>
//   depends on: Input
define_intermediate!(Parse, parse, cloned Result<Ast, Error>, parse_program);

// fn check(ast: Rc<Ast>, env: Rc<CheckEnv>, db) -> Result<(), Error>
//   depends on: Check(subtree, _)
define_intermediate!(Check { &ast: Rc<Ast>, &env: Rc<CheckEnv> }, check, cloned Result<(), Error>, check_impl);

// fn execute(ast: Rc<Ast>, env: Rc<ExecEnv>, db) -> Result<i64, Error>
//   depends on: Execute(subtree, _)
define_intermediate!(Execute { &ast: Rc<Ast>, &env: Rc<ExecEnv> }, execute, cloned Result<i64, Error>, execute_impl);

// fn execute_all(db) -> Result<i64, Error>
//   depends on: Check(..), Execute(..)
define_intermediate!(ExecuteAll, execute_all, cloned Result<i64, Error>, execute_all_impl);

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

#[derive(Debug, Hash, PartialEq, Eq, Clone, PartialOrd, Ord)]
enum Ast {
    Var {
        name: String,
    },
    Int(i64),
    Add(Rc<Ast>, Rc<Ast>),
    Let {
        name: String,
        rhs: Rc<Ast>,
        body: Rc<Ast>,
    },
}

type CheckEnv = BTreeSet<String>;
type ExecEnv = BTreeMap<String, i64>;

/// Parse an s-expr:
///
/// program: expr
/// expr: variable | integer | add | let
/// variable: [A-Za-z_]+
/// integer: -?[0-9]
/// add: '(' '+' expr expr ')'
/// let: '(' 'let' variable expr expr ')'
fn parse_program(db: &mut DbHandle<impl Computation>) -> Result<Ast, Error> {
    let program: &String = get_input(db);
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
                return Err(Error::UnterminatedLParen(ast));
            } else {
                text = &text[1..];
                ast
            }
        }
        s if s.is_ascii_alphabetic() => {
            let (word, rest) = parse_word(text);
            text = rest;
            Ast::Var {
                name: word.to_string(),
            }
        }
        s if s.is_numeric() => {
            let (word, rest) = parse_word(text);
            text = rest;
            let int = word
                .parse::<i64>()
                .map_err(|_| Error::InvalidIntegerLiteral(word.to_string()))?;
            Ast::Int(int)
        }
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
fn check_impl(
    ast: &Ast,
    env: &Rc<CheckEnv>,
    db: &mut DbHandle<impl Computation>,
) -> Result<(), Error> {
    match ast {
        Ast::Var { name } => {
            if env.contains(name) {
                Ok(())
            } else {
                Err(Error::NameNotDefined(name.clone()))
            }
        }
        Ast::Int(_) => Ok(()),
        Ast::Add(lhs, rhs) => {
            check(lhs.clone(), env.clone(), db)?;
            check(rhs.clone(), env.clone(), db)
        }
        Ast::Let { name, rhs, body } => {
            check(rhs.clone(), env.clone(), db)?;

            let mut new_env = env.as_ref().clone();
            new_env.insert(name.clone());
            check(body.clone(), Rc::new(new_env), db)
        }
    }
}

fn execute_impl(
    ast: &Rc<Ast>,
    env: &Rc<ExecEnv>,
    db: &mut DbHandle<impl Computation>,
) -> Result<i64, Error> {
    match ast.as_ref() {
        Ast::Var { name } => {
            // Assume `check` has already been run and thus that all names are defined
            Ok(env[name])
        }
        Ast::Int(x) => Ok(*x),
        Ast::Add(lhs, rhs) => {
            let lhs = execute(lhs.clone(), env.clone(), db)?;
            let rhs = execute(rhs.clone(), env.clone(), db)?;
            Ok(lhs + rhs)
        }
        Ast::Let { name, rhs, body } => {
            let rhs = execute(rhs.clone(), env.clone(), db)?;

            let mut new_env = env.as_ref().clone();
            new_env.insert(name.clone(), rhs);
            execute(body.clone(), Rc::new(new_env), db)
        }
    }
}

/// Without a separate execute_all rule, `execute` would need to call `check`
/// recursively for each sub-tree - at every level of recursion. While in practice the result would
/// be cached which would prevent exponential slowdowns, checking if the dependency is up-to-date
/// at each recursive step is still wasteful. So a separate rule is used here to run the entire
/// check pass, followed by the entire execute pass which resembles how a typical compiler is
/// structured.
fn execute_all_impl(db: &mut DbHandle<impl Computation>) -> Result<i64, Error> {
    let ast = parse(db)?;
    let ast = Rc::new(ast);
    check(ast.clone(), Rc::new(CheckEnv::new()), db)?;
    execute(ast.clone(), Rc::new(ExecEnv::new()), db)
}

fn set_input(db: &mut Db<Compiler>, source_program: &str) {
    db.update_input(Input::default(), source_program.to_string());
}

mod compiler {
    use crate::*;
    use inc_complete::Db;

    #[test]
    fn basic_programs() {
        let mut db = Db::<Compiler>::new();

        set_input(&mut db, "42");
        let result = execute_all_db(&mut db);
        assert_eq!(result, Ok(42));

        set_input(&mut db, "(+ 42 58)");
        let result = execute_all_db(&mut db);
        assert_eq!(result, Ok(100));

        set_input(&mut db, "(let foo 42 (+ 58 foo))");
        let result = execute_all_db(&mut db);
        assert_eq!(result, Ok(100));

        set_input(&mut db, "(let foo 42 (+ 58 foo)) foo");
        let result = execute_all_db(&mut db);
        assert_eq!(
            result,
            Err(Error::InputEmptyOrUnparsedOutput(" foo".to_string()))
        );

        set_input(&mut db, "(let foo 42 (+ foo bar))");
        let result = execute_all_db(&mut db);
        assert_eq!(result, Err(Error::NameNotDefined("bar".to_string())));

        set_input(&mut db, "(let foo 42 (let bar 8 (+ foo bar)))");
        let result = execute_all_db(&mut db);
        assert_eq!(result, Ok(50));
    }

    #[test]
    fn cached() {
        let mut db = Db::<Compiler>::new();

        set_input(&mut db, "(+ 42 58)");
        let result = execute_all_db(&mut db);
        assert_eq!(result, Ok(100));

        // Update the input, aiming to re-use a previous computation
        set_input(&mut db, "42");
        let ast = parse_db(&mut db).unwrap();
        assert_eq!(ast, Ast::Int(42));

        // Although the input has changed, we cache each intermediate result and shouldn't
        // need to re-run execute (but execute_all will re-run to produce an Ast that we
        // can check is cached).
        //
        // Note that since the execution environment is cached as well if that had changed
        // then we would still need to re-run this. This is desired for Asts containing Var
        // nodes but not desired for this simple Int node. If we wanted to, we could fix this
        // by adding new rules for ExecuteInt and CheckInt which don't require an environment.
        let ast = Rc::new(ast);
        assert!(!db.is_stale(&Check(ast.clone(), Default::default())));
        assert!(!db.is_stale(&Execute(ast, Default::default())));
    }
}
