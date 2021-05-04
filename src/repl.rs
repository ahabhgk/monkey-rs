use crate::lexer::Lexer;

use std::io;

const PROMPT: &'static str = ">> ";

pub fn start<R: io::BufRead, W: io::Write>(mut reader: R, mut writer: W) -> io::Result<()> {
    loop {
        writer.write(PROMPT.as_bytes())?;
        writer.flush()?;
        let mut line = String::new();
        reader.read_line(&mut line)?;
        let l = Lexer::new(&line);

        for tok in l {
            println!("{:?}", tok)
        }
    }
}
