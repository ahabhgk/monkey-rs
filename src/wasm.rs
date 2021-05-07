use crate::{environment::Environment, evaluator::Evaluator, lexer::Lexer, parser::Parser};
use std::rc::Rc;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn run(code: &str) -> String {
    let env = Environment::new();
    let l = Lexer::new(code);
    let mut p = Parser::new(l);

    let program = p.parse_program();
    let errors = p.errors();
    if errors.len() != 0 {
        return errors.join("\n").to_string();
    }

    match Evaluator::eval(&program, Rc::clone(&env)) {
        Ok(evaluated) => evaluated.to_string(),
        Err(err) => format!("error: {}", err),
    }
}
