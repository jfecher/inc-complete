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
    ///   depends on: Parse
    Check(Rc<Ast>, Rc<CheckEnv>),

    /// fn execute(ast: Rc<Ast>, env: Rc<Env>) -> Result<i64, Error>
    ///   depends on: Check, Execute(subtree, _)
    Execute(Rc<Ast>, Rc<ExecEnv>)
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
enum Error {
    IncorrectArgumentCount,
    InvalidIntegerLiteral(String),
    UnexpectedChar(char),
    LetVarIsNotAnIdent(Ast),
    InputEmptyOrUnparsedOutput,

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
    let mut values = Vec::new();
    let program: &String = db.get(Compiler::Input);

    let program = program.trim();
    for word in program.split_whitespace() {
        let Some(first_char) = word.chars().next() else {
            continue;
        };

        match first_char {
            'A'..'Z' | 'a'..'z' | '_' => {
                if word == "let" {
                    let body= Rc::new(values.pop().ok_or(Error::IncorrectArgumentCount)?);
                    let name = values.pop().ok_or(Error::IncorrectArgumentCount)?;
                    let rhs = Rc::new(values.pop().ok_or(Error::IncorrectArgumentCount)?);

                    let Ast::Var { name } = name else {
                        return Err(Error::LetVarIsNotAnIdent(name));
                    };

                    values.push(Ast::Let { name, rhs, body });
                } else {
                    values.push(Ast::Var { name: word.to_string() });
                }
            },
            '0'..'9' | '-' => {
                let int = word.parse::<i64>().map_err(|_| Error::InvalidIntegerLiteral(word.to_string()))?;
                values.push(Ast::Int(int));
            }
            '+' => {
                let b = values.pop().ok_or(Error::IncorrectArgumentCount)?;
                let a = values.pop().ok_or(Error::IncorrectArgumentCount)?;
                values.push(Ast::Add(Rc::new(a), Rc::new(b)));
            }
            other => return Err(Error::UnexpectedChar(other)),
        }
    }

    if values.len() == 1 {
        Ok(values.pop().unwrap())
    } else {
        Err(Error::InputEmptyOrUnparsedOutput)
    }
}

/// Given an s-expr call `(f a (b c) d)` return `vec!["f", "a", "(b c)", "d"]`
/// Errors if the parenthesis are mismatched
fn parse_call(text: &str) -> Result<Vec<&str>, Error> {
    let mut index = 0;

    for char in text.chars() {
        match char {
            '(' => parse_call(substring),
            s if s.is_whitespace() => (),
            s if s.is_ascii_alphabetic() => (),
            s if s.is_numeric() => (),
            _ => (),
        }
    }
}

/// Ensure all variables are defined or return an Error
/// FIXME: Currently we're required to clone each sub-tree and environment.
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
    // Ensure all names are defined
    let check_env = env.iter().map(|(key, _)| key.clone()).collect();
    query_check(ast.clone(), Rc::new(check_env), db)?;

    match ast.as_ref() {
        Ast::Var { name } => {
            // We already checked that all names are defined
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

/// Helper to execute with the full input
fn execute_all(db: &mut Db<Compiler>) -> Result<i64, Error> {
    let ast = db.get::<Result<Ast, Error>>(Compiler::Parse).clone()?;
    Clone::clone(db.get(Compiler::Execute(Rc::new(ast), Rc::new(ExecEnv::new()))))
}

fn set_input(db: &mut Db<Compiler>, source_program: &str) {
    db.update_input(Compiler::Input, Value::new(source_program.to_string()));
}

#[test]
fn basic_programs() {
    let mut db = Db::<Compiler>::new();

    set_input(&mut db, "42");
    let result = execute_all(&mut db);
    assert_eq!(result, Ok(42));

    set_input(&mut db, "42 58 +");
    let result = execute_all(&mut db);
    assert_eq!(result, Ok(100));

    set_input(&mut db, "42 foo 58 foo + let");
    let result = execute_all(&mut db);
    assert_eq!(result, Ok(100));

    set_input(&mut db, "42 foo 58 foo + let foo");
    let result = execute_all(&mut db);
    assert_eq!(result, Ok(100));

    set_input(&mut db, "42 foo 8 bar foo bar + let let");
    let result = execute_all(&mut db);
    assert_eq!(result, Ok(50));
}
