use std::{collections::{BTreeMap, BTreeSet}, rc::Rc};

use crate::{Cached, Computation, Db, DbHandle, OutputTypeForInput, Run};

/// Test a somewhat more complex case with nested computations
/// There is a lot of cloning required to create these keys. This can be
/// improved in the future with interning. For now we manually wrap arguments
/// in `Rc` to reduce the cost a bit. This still requires cloning when - e.g.
/// the argument is mutated such as with the environment parameters.
type Compiler = (
    crate::Input<Input>,
    Cached<Parse>,
    Cached<Check>,
    Cached<Execute>,
    Cached<ExecuteAll>,
);

/// Input: String
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Input;
const INPUT: crate::Input<Input> = crate::Input::new();

impl OutputTypeForInput for Input {
    type Output = String;
}

/// fn parse() -> Result<Ast, Error>
///   depends on: Input
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Parse;
const PARSE: Cached<Parse> = Cached::new(Parse);

impl Run for Parse {
    type Output = Result<Ast, Error>;

    fn run(&self, handle: &mut DbHandle<impl crate::Computation>) -> Self::Output {
        parse_program(handle)
    }
}

/// fn check(ast: Rc<Ast>, env: Rc<CheckEnv>) -> Result<(), Error>
///   depends on: Check(subtree, _)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Check(Rc<Ast>, Rc<CheckEnv>);

impl Check {
    fn new(ast: Rc<Ast>, env: Rc<CheckEnv>) -> Cached<Check> {
        Cached::new(Self(ast, env))
    }
}

impl Run for Check {
    type Output = Result<(), Error>;

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
        check(&self.0, &self.1, handle)
    }
}

/// fn execute(ast: Rc<Ast>, env: Rc<Env>) -> Result<i64, Error>
///   depends on: Execute(subtree, _)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Execute(Rc<Ast>, Rc<ExecEnv>);

impl Execute {
    fn new(ast: Rc<Ast>, env: Rc<ExecEnv>) -> Cached<Execute> {
        Cached::new(Self(ast, env))
    }
}

impl Run for Execute {
    type Output = Result<i64, Error>;

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
        execute(&self.0, &self.1, handle)
    }
}

/// fn execute_all() -> Result<i64, Error>
///   depends on: Check(..), Execute(..)
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ExecuteAll;
const EXECUTE_ALL: Cached<ExecuteAll> = Cached::new(ExecuteAll);

impl Run for ExecuteAll {
    type Output = Result<i64, Error>;

    fn run(&self, handle: &mut DbHandle<impl Computation>) -> Self::Output {
        execute_all(handle)
    }
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

/// Parse an s-expr:
///
/// program: expr
/// expr: variable | integer | add | let
/// variable: [A-Za-z_]+
/// integer: -?[0-9]
/// add: '(' '+' expr expr ')'
/// let: '(' 'let' variable expr expr ')'
fn parse_program(db: &mut DbHandle<impl Computation>) -> Result<Ast, Error> {
    let program: &String = db.get(INPUT);
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
fn check(ast: &Ast, env: &Rc<CheckEnv>, db: &mut DbHandle<impl Computation>) -> Result<(), Error> {
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
            db.get(Check::new(lhs.clone(), env.clone())).clone()?;
            db.get(Check::new(rhs.clone(), env.clone())).clone()
        },
        Ast::Let { name, rhs, body } => {
            db.get(Check::new(rhs.clone(), env.clone())).clone()?;

            let mut new_env = env.as_ref().clone();
            new_env.insert(name.clone());
            db.get(Check::new(body.clone(), Rc::new(new_env))).clone()
        },
    }
}

fn execute(ast: &Rc<Ast>, env: &Rc<ExecEnv>, db: &mut DbHandle<impl Computation>) -> Result<i64, Error> {
    match ast.as_ref() {
        Ast::Var { name } => {
            // Assume `check` has already been run and thus that all names are defined
            Ok(env[name])
        },
        Ast::Int(x) => Ok(*x),
        Ast::Add(lhs, rhs) => {
            let lhs = db.get(Execute::new(lhs.clone(), env.clone())).clone()?;
            let rhs = db.get(Execute::new(rhs.clone(), env.clone())).clone()?;
            Ok(lhs + rhs)
        },
        Ast::Let { name, rhs, body } => {
            let rhs = db.get(Execute::new(rhs.clone(), env.clone())).clone()?;

            let mut new_env = env.as_ref().clone();
            new_env.insert(name.clone(), rhs);
            db.get(Execute::new(body.clone(), Rc::new(new_env))).clone()
        },
    }
}

/// Without a separate execute_all rule, `execute` would need to call `check`
/// recursively for each sub-tree - at every level of recursion. While in practice the result would
/// be cached which would prevent exponential slowdowns, checking if the dependency is up-to-date
/// at each recursive step is still wasteful. So a separate rule is used here to run the entire
/// check pass, followed by the entire execute pass which resembles how a typical compiler is
/// structured.
fn execute_all(db: &mut DbHandle<impl Computation>) -> Result<i64, Error> {
    let ast = db.get(PARSE).clone()?;
    let ast = Rc::new(ast);

    db.get(Check::new(ast.clone(), Rc::new(CheckEnv::new()))).clone()?;
    db.get(Execute::new(ast.clone(), Rc::new(ExecEnv::new()))).clone()
}

fn set_input(db: &mut Db<Compiler>, source_program: &str) {
    db.update_input(INPUT, source_program.to_string());
}

#[test]
fn basic_programs() {
    let mut db = Db::<Compiler>::new();

    set_input(&mut db, "42");
    let result = db.get(EXECUTE_ALL).clone();
    assert_eq!(result, Ok(42));

    set_input(&mut db, "(+ 42 58)");
    let result = db.get(EXECUTE_ALL).clone();
    assert_eq!(result, Ok(100));

    set_input(&mut db, "(let foo 42 (+ 58 foo))");
    let result = db.get(EXECUTE_ALL).clone();
    assert_eq!(result, Ok(100));

    set_input(&mut db, "(let foo 42 (+ 58 foo)) foo");
    let result = db.get(EXECUTE_ALL).clone();
    assert_eq!(result, Err(Error::InputEmptyOrUnparsedOutput(" foo".to_string())));

    set_input(&mut db, "(let foo 42 (+ foo bar))");
    let result = db.get(EXECUTE_ALL).clone();
    assert_eq!(result, Err(Error::NameNotDefined("bar".to_string())));

    set_input(&mut db, "(let foo 42 (let bar 8 (+ foo bar)))");
    let result = db.get(EXECUTE_ALL).clone();
    assert_eq!(result, Ok(50));
}

#[test]
fn cached() {
    let mut db = Db::<Compiler>::new();

    set_input(&mut db, "(+ 42 58)");
    let result = db.get(EXECUTE_ALL).clone();
    assert_eq!(result, Ok(100));

    // Update the input, aiming to re-use a previous computation
    set_input(&mut db, "42");
    let ast = db.get(PARSE).clone().unwrap();
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
    assert!(!db.is_stale(&Check::new(ast.clone(), Default::default())));
    assert!(!db.is_stale(&Execute::new(ast, Default::default())));
}
