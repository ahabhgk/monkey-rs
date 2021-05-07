use crate::token::{lookup_ident, Token};
use std::{iter::Peekable, str::Chars};

fn is_letter(ch: char) -> bool {
    ch.is_alphabetic() || ch == '_'
}

fn is_digital(ch: char) -> bool {
    ch.is_digit(10)
}

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn peek_is_whitespace(&mut self) -> bool {
        match self.peek_char() {
            Some(ch) => ch.is_whitespace(),
            None => false,
        }
    }

    fn peek_is_letter(&mut self) -> bool {
        match self.peek_char() {
            Some(&ch) => is_letter(ch),
            None => false,
        }
    }

    fn peek_is_number(&mut self) -> bool {
        match self.peek_char() {
            Some(&ch) => is_digital(ch),
            None => false,
        }
    }

    fn skip_whitespace(&mut self) {
        while self.peek_is_whitespace() {
            self.read_char();
        }
    }

    fn read_string(&mut self) -> Token {
        let mut s = String::new();
        loop {
            match self.read_char() {
                Some('"') | None => break,
                Some(ch) => s.push(ch),
            }
        }
        Token::STRING(s)
    }

    fn read_identifier(&mut self, ch: char) -> Token {
        let mut ident = String::new();
        ident.push(ch);
        while self.peek_is_letter() {
            match self.read_char() {
                Some(ch) => ident.push(ch),
                None => break,
            };
        }
        lookup_ident(ident)
    }

    fn read_number(&mut self, ch: char) -> Token {
        let mut number = String::new();
        number.push(ch);
        while self.peek_is_number() {
            match self.read_char() {
                Some(ch) => number.push(ch),
                None => break,
            };
        }

        Token::INT(number)
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.read_char() {
            Some('=') => {
                if let Some(&'=') = self.peek_char() {
                    self.read_char();
                    Token::EQ
                } else {
                    Token::ASSIGN
                }
            }
            Some('+') => Token::PLUS,
            Some('-') => Token::MINUS,
            Some('!') => {
                if let Some(&'=') = self.peek_char() {
                    self.read_char();
                    Token::NEQ
                } else {
                    Token::BANG
                }
            }
            Some('/') => Token::SLASH,
            Some('*') => Token::ASTERISK,
            Some('<') => Token::LT,
            Some('>') => Token::GT,
            Some(';') => Token::SEMICOLON,
            Some(',') => Token::COMMA,
            Some(':') => Token::COLON,
            Some('(') => Token::LPAREN,
            Some(')') => Token::RPAREN,
            Some('{') => Token::LBRACE,
            Some('}') => Token::RBRACE,
            Some('[') => Token::LBRACKET,
            Some(']') => Token::RBRACKET,
            Some('"') => self.read_string(),
            Some(ch) => {
                if is_letter(ch) {
                    self.read_identifier(ch)
                } else if is_digital(ch) {
                    self.read_number(ch)
                } else {
                    Token::ILLEGAL
                }
            }
            None => Token::EOF,
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        if tok == Token::EOF {
            None
        } else {
            Some(tok)
        }
    }
}

#[cfg(test)]
mod lexer_test {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "=+(){},;";
        let tests = vec![
            Token::ASSIGN,
            Token::PLUS,
            Token::LPAREN,
            Token::RPAREN,
            Token::LBRACE,
            Token::RBRACE,
            Token::COMMA,
            Token::SEMICOLON,
            Token::EOF,
        ];

        let mut l = Lexer::new(input);
        for test in &tests {
            let tok = l.next_token();
            assert_eq!(tok, *test);
        }
    }

    #[test]
    fn test_next_token_statements() {
        let input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
";

        let tests = vec![
            Token::LET,
            Token::IDENT("five".to_string()),
            Token::ASSIGN,
            Token::INT("5".to_string()),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("ten".to_string()),
            Token::ASSIGN,
            Token::INT("10".to_string()),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("add".to_string()),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT("x".to_string()),
            Token::COMMA,
            Token::IDENT("y".to_string()),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT("x".to_string()),
            Token::PLUS,
            Token::IDENT("y".to_string()),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT("result".to_string()),
            Token::ASSIGN,
            Token::IDENT("add".to_string()),
            Token::LPAREN,
            Token::IDENT("five".to_string()),
            Token::COMMA,
            Token::IDENT("ten".to_string()),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::EOF,
        ];

        let mut l = Lexer::new(input);
        for test in &tests {
            let tok = l.next_token();
            assert_eq!(tok, *test);
        }
    }

    #[test]
    fn test_next_token_extended() {
        let input = "!-/*5;
5 < 10 > 5;
if ( 5 < 10 ) {
    return true;
} else {
    return false;
}
10 == 10;
10 != 9;
";

        let tests = vec![
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT("5".to_string()),
            Token::SEMICOLON,
            Token::INT("5".to_string()),
            Token::LT,
            Token::INT("10".to_string()),
            Token::GT,
            Token::INT("5".to_string()),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT("5".to_string()),
            Token::LT,
            Token::INT("10".to_string()),
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT("10".to_string()),
            Token::EQ,
            Token::INT("10".to_string()),
            Token::SEMICOLON,
            Token::INT("10".to_string()),
            Token::NEQ,
            Token::INT("9".to_string()),
            Token::SEMICOLON,
            Token::EOF,
        ];

        let mut l = Lexer::new(input);
        for test in &tests {
            let tok = l.next_token();
            assert_eq!(tok, *test);
        }
    }

    #[test]
    fn test_next_token_string() {
        let input = "\"foobar\";
\"foo bar\";
";

        let tests = vec![
            Token::STRING("foobar".to_string()),
            Token::SEMICOLON,
            Token::STRING("foo bar".to_string()),
            Token::SEMICOLON,
            Token::EOF,
        ];

        let mut l = Lexer::new(input);
        for test in &tests {
            let tok = l.next_token();
            assert_eq!(tok, *test);
        }
    }

    #[test]
    fn test_next_token_bracket() {
        let input = "[1,2];
{\"foo\": \"bar\"}
";

        let tests = vec![
            Token::LBRACKET,
            Token::INT("1".to_string()),
            Token::COMMA,
            Token::INT("2".to_string()),
            Token::RBRACKET,
            Token::SEMICOLON,
            Token::LBRACE,
            Token::STRING("foo".to_string()),
            Token::COLON,
            Token::STRING("bar".to_string()),
            Token::RBRACE,
            Token::EOF,
        ];

        let mut l = Lexer::new(input);
        for test in &tests {
            let tok = l.next_token();
            assert_eq!(tok, *test);
        }
    }
}
