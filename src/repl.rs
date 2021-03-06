use crate::{
    environment::Environment, evaluator::Evaluator, lexer::Lexer,
    parser::Parser,
};
use std::{io, rc::Rc};

const PROMPT: &'static str = ">> ";

pub fn start<R: io::BufRead, W: io::Write>(
    mut reader: R,
    mut writer: W,
) -> io::Result<()> {
    let env = Environment::new();
    loop {
        writer.write(PROMPT.as_bytes())?;
        writer.flush()?;
        let mut line = String::new();
        reader.read_line(&mut line)?;

        let l = Lexer::new(&line);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let errors = p.errors();
        if errors.len() != 0 {
            for error in errors {
                write!(writer, "\t{}\t\n", error)?;
            }
            continue;
        }

        match Evaluator::eval(&program, Rc::clone(&env)) {
            Ok(evaluated) => write!(writer, "{}\n", evaluated)?,
            Err(err) => write!(writer, "error: {}\n", err)?,
        };
    }
}
