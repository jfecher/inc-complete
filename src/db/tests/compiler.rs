// use std::collections::HashMap;
// 
// use crate::{DbHandle, Run, Value};
// 
// Test a somewhat more complex case with nested computations
// #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
// enum Compiler {
//     /// fn parse(file_contents: String) -> Result<Ast, Error>
//     Parse(String),
// 
//     /// fn check(ast: Ast) -> Result<(), Error>
//     Check(Ast),
// 
//     /// fn execute(ast: Ast, env: Env) -> Result<i64, Error>
//     Execute(Ast, Env)
// }
// 
// #[derive(Hash, PartialEq, Eq)]
// enum Error {
//     ParseError,
//     TypeError,
//     NameNotDefined(String),
// }
// 
// #[derive(Hash, PartialEq, Eq)]
// enum Ast {
//     Var { name: String },
//     Int(i64),
//     Add(Box<Ast>, Box<Ast>),
//     Let { name: String, rhs: Box<Ast>, body: Box<Ast> },
// }
// 
// type Env = HashMap<String, i64>;
// 
// impl Run for Compiler {
//     fn run(self, db: &mut DbHandle<Self>) -> Value {
//         use Compiler::*;
//         match self {
//             Parse(program) => Value::new(parse_program(program)),
//             Check(ast) => Value::new(check(ast, db)),
//             Execute(ast, env) => Value::new(execute(ast, env, db)),
//         }
//     }
// }
// 
// /// For ease of implementation, the grammar of the program is that
// /// of reverse polish notation (RPN) with variables, integers, +, and `=` for defining variables.
// ///
// /// program: expr+
// /// expr: variable | integer | add | let
// /// variable: [A-Za-z_]+
// /// integer: -?[0-9]
// /// add: expr expr '+'
// /// let: expr variable expr '='
// ///
// /// `let` is a bit weird. The syntax is the expression the variable is set equal to,
// /// followed by the variable name, followed by the body in which it is defined in,
// /// followed by `=`.
// fn parse_program(program: String) -> Result<Ast, Error> {
//     let mut values = Vec::new();
// 
//     let program = program.trim();
//     for word in program.split_whitespace() {
//         let Some(first_char) = word.chars().next() else {
//             continue;
//         };
// 
//         match first_char {
//             'A'..'Z' | 'a'..'z' | '_' => {
//                 if word == "let" {
//                     let body= Box::new(values.pop().ok_or(Error::ParseError)?);
//                     let name = values.pop().ok_or(Error::ParseError)?;
//                     let rhs = Box::new(values.pop().ok_or(Error::ParseError)?);
// 
//                     let Ast::Var { name } = name else {
//                         return Err(Error::ParseError);
//                     };
// 
//                     values.push(Ast::Let { name, rhs, body });
//                 } else {
//                     values.push(Ast::Var { name: word.to_string() });
//                 }
//             },
//             '0'..'9' | '-' => {
//                 let int = word.parse::<i64>().map_err(|_| Error::ParseError)?;
//                 values.push(Ast::Int(int));
//             }
//             '+' => {
//                 let b = values.pop().ok_or(Error::ParseError)?;
//                 let a = values.pop().ok_or(Error::ParseError)?;
//                 values.push(Ast::Add(Box::new(a), Box::new(b)));
//             }
//             _ => return Err(Error::ParseError),
//         }
//     }
// 
//     if values.len() == 1 {
//         Ok(values.pop().unwrap())
//     } else {
//         // input is empty or there was extra unparsed output
//         Err(Error::ParseError)
//     }
// }
// 
// fn check(_ast: Ast, _db: &mut DbHandle<Compiler>) -> Result<(), Error> {
//     todo!()
// }
// 
// fn execute(_ast: Ast, _env: HashMap<String, i64>, _db: &mut DbHandle<'_, Compiler>) -> Result<i64, Error> {
//     todo!()
// }
