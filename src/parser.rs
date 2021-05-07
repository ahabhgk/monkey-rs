use crate::{
    ast::{BlockStatement, Expression, HashLiteral, Infix, Prefix, Program, Statement},
    lexer::Lexer,
    token::Token,
};
use std::{collections::HashMap, iter::Peekable};

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    LOWEST = 0,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunc(X)
    INDEX,       // array[index]
}

impl From<&Token> for Precedence {
    fn from(token: &Token) -> Self {
        match token {
            Token::EQ | Token::NEQ => Precedence::EQUALS,
            Token::LT | Token::GT => Precedence::LESSGREATER,
            Token::PLUS | Token::MINUS => Precedence::SUM,
            Token::SLASH | Token::ASTERISK => Precedence::PRODUCT,
            Token::LPAREN => Precedence::CALL,
            Token::LBRACKET => Precedence::INDEX,
            _ => Precedence::LOWEST,
        }
    }
}

type PrefixParseFn = fn(parser: &mut Parser) -> Option<Expression>;
type InfixParseFn = fn(parser: &mut Parser, left: Expression) -> Option<Expression>;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    cur_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Parser {
            lexer: lexer.peekable(),
            cur_token: Token::EOF,
            errors: Vec::new(),
        };

        p.next_token();
        p
    }

    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    fn next_token(&mut self) {
        self.cur_token = match self.lexer.next() {
            Some(tok) => tok,
            None => Token::EOF,
        }
    }

    fn expect_peek(&mut self, expected: &Token) -> bool {
        match self.lexer.peek() {
            Some(&Token::IDENT(_)) => match expected {
                Token::IDENT(_) => {
                    self.next_token();
                    true
                }
                _ => {
                    self.errors.push(format!(
                        "expected next token to be {:?}, got IDENT instead",
                        expected,
                    ));
                    false
                }
            },
            Some(tok) => {
                if tok == expected {
                    self.next_token();
                    true
                } else {
                    self.errors.push(format!(
                        "expected next token to be {:?}, got {:?} instead",
                        expected, tok
                    ));
                    false
                }
            }
            None => {
                self.errors.push(format!(
                    "expected next token to be {:?}, got None instead",
                    expected
                ));
                false
            }
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token != Token::EOF {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
    }

    #[inline]
    fn prefix_parse_fn(token: &Token) -> Option<PrefixParseFn> {
        match token {
            Token::IDENT(_) => Some(Parser::parse_identifier),
            Token::INT(_) => Some(Parser::parse_integer),
            Token::STRING(_) => Some(Parser::parse_string),
            Token::LBRACKET => Some(Parser::parse_array),
            Token::BANG | Token::MINUS => Some(Parser::parse_prefix),
            Token::TRUE | Token::FALSE => Some(Parser::parse_boolean),
            Token::LPAREN => Some(Parser::parse_parenthesis),
            Token::LBRACE => Some(Parser::parse_hash),
            Token::IF => Some(Parser::parse_if),
            Token::FUNCTION => Some(Parser::parse_function),
            _ => None,
        }
    }

    #[inline]
    fn infix_parse_fn(token: &Token) -> Option<InfixParseFn> {
        match token {
            Token::EQ
            | Token::NEQ
            | Token::LT
            | Token::GT
            | Token::PLUS
            | Token::MINUS
            | Token::SLASH
            | Token::ASTERISK => Some(Parser::parse_infix),
            Token::LPAREN => Some(Parser::parse_call),
            Token::LBRACKET => Some(Parser::parse_index),
            _ => None,
        }
    }

    fn parse_identifier(parser: &mut Parser) -> Option<Expression> {
        match &parser.cur_token {
            Token::IDENT(ident) => Some(Expression::Identifier(ident.to_string())),
            _ => None,
        }
    }

    fn parse_integer(parser: &mut Parser) -> Option<Expression> {
        match &parser.cur_token {
            Token::INT(int) => match int.parse::<isize>() {
                Ok(i) => Some(Expression::Integer(i)),
                _ => None,
            },
            _ => None,
        }
    }

    fn parse_string(parser: &mut Parser) -> Option<Expression> {
        match &parser.cur_token {
            Token::STRING(string) => Some(Expression::String(string.to_string())),
            _ => None,
        }
    }

    fn parse_array(parser: &mut Parser) -> Option<Expression> {
        match parser.parse_expression_list(&Token::RBRACKET) {
            Some(array) => Some(Expression::Array(array)),
            _ => None,
        }
    }

    fn parse_boolean(parser: &mut Parser) -> Option<Expression> {
        match &parser.cur_token {
            Token::TRUE => Some(Expression::Boolean(true)),
            Token::FALSE => Some(Expression::Boolean(false)),
            _ => None,
        }
    }

    fn parse_prefix(parser: &mut Parser) -> Option<Expression> {
        let prefix = match &parser.cur_token {
            Token::BANG => Prefix::BANG,
            Token::MINUS => Prefix::MINUS,
            _ => return None,
        };

        parser.next_token();

        match parser.parse_expression(Precedence::PREFIX) {
            Some(right) => Some(Expression::Prefix(prefix, Box::new(right))),
            _ => None,
        }
    }

    fn parse_infix(parser: &mut Parser, left: Expression) -> Option<Expression> {
        let infix = match &parser.cur_token {
            Token::PLUS => Infix::PLUS,
            Token::MINUS => Infix::MINUS,
            Token::ASTERISK => Infix::ASTERISK,
            Token::SLASH => Infix::SLASH,
            Token::LT => Infix::LT,
            Token::GT => Infix::GT,
            Token::EQ => Infix::EQ,
            Token::NEQ => Infix::NEQ,
            _ => return None,
        };

        let cur_precedence = Precedence::from(&parser.cur_token);
        parser.next_token();

        match parser.parse_expression(cur_precedence) {
            Some(right) => Some(Expression::Infix(Box::new(left), infix, Box::new(right))),
            _ => None,
        }
    }

    fn parse_index(parser: &mut Parser, left: Expression) -> Option<Expression> {
        parser.next_token();

        if let Some(right) = parser.parse_expression(Precedence::LOWEST) {
            if !parser.expect_peek(&Token::RBRACKET) {
                return None;
            }

            Some(Expression::Index(Box::new(left), Box::new(right)))
        } else {
            None
        }
    }

    fn parse_call(parser: &mut Parser, left: Expression) -> Option<Expression> {
        match parser.parse_expression_list(&Token::RPAREN) {
            Some(args) => Some(Expression::Call(Box::new(left), args)),
            _ => None,
        }
    }

    fn parse_if(parser: &mut Parser) -> Option<Expression> {
        if !parser.expect_peek(&Token::LPAREN) {
            return None;
        }

        parser.next_token();

        let condition = parser.parse_expression(Precedence::LOWEST);
        if condition.is_none() {
            return None;
        }

        if !parser.expect_peek(&Token::RPAREN) {
            return None;
        }

        if !parser.expect_peek(&Token::LBRACE) {
            return None;
        }

        let consequence = parser.parse_block_statement();

        let alternative = if let Some(token) = parser.lexer.peek() {
            match token {
                Token::ELSE => {
                    parser.next_token();
                    if !parser.expect_peek(&Token::LBRACE) {
                        return None;
                    }

                    Some(parser.parse_block_statement())
                }
                _ => None,
            }
        } else {
            None
        };

        Some(Expression::If(
            Box::new(condition.unwrap()),
            consequence,
            alternative,
        ))
    }

    fn parse_parenthesis(parser: &mut Parser) -> Option<Expression> {
        parser.next_token();
        let expr = parser.parse_expression(Precedence::LOWEST);

        if !parser.expect_peek(&Token::RPAREN) {
            return None;
        }

        expr
    }

    fn parse_hash(parser: &mut Parser) -> Option<Expression> {
        let mut hash = HashLiteral {
            pairs: HashMap::new(),
        };

        while let Some(token) = parser.lexer.peek() {
            if *token == Token::RBRACE {
                break;
            }

            parser.next_token();
            let key = parser.parse_expression(Precedence::LOWEST);
            if key.is_none() {
                return None;
            }

            if !parser.expect_peek(&Token::COLON) {
                return None;
            }

            parser.next_token();
            let value = parser.parse_expression(Precedence::LOWEST);
            if value.is_none() {
                return None;
            }

            hash.pairs.insert(key.unwrap(), value.unwrap());

            if let Some(token) = parser.lexer.peek() {
                if *token != Token::RBRACE && !parser.expect_peek(&Token::COMMA) {
                    return None;
                }
            }
        }

        if !parser.expect_peek(&Token::RBRACE) {
            return None;
        }

        Some(Expression::Hash(hash))
    }

    fn parse_function(parser: &mut Parser) -> Option<Expression> {
        if !parser.expect_peek(&Token::LPAREN) {
            return None;
        }

        let params = parser.parse_function_parameters();
        if params.is_none() {
            return None;
        }

        if !parser.expect_peek(&Token::LBRACE) {
            return None;
        }

        let body = parser.parse_block_statement();

        Some(Expression::Function(params.unwrap(), body))
    }

    fn parse_expression_list(&mut self, expected: &Token) -> Option<Vec<Expression>> {
        let mut args = vec![];

        if let Some(token) = self.lexer.peek() {
            if token == expected {
                self.next_token();
                return Some(args);
            }
        }

        self.next_token();
        if let Some(arg) = self.parse_expression(Precedence::LOWEST) {
            args.push(arg);
        } else {
            return None;
        }

        while let Some(token) = self.lexer.peek() {
            if *token != Token::COMMA {
                break;
            }
            self.next_token();
            self.next_token();
            if let Some(arg) = self.parse_expression(Precedence::LOWEST) {
                args.push(arg);
            } else {
                return None;
            }
        }

        if !self.expect_peek(expected) {
            return None;
        }

        Some(args)
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let opt_left = match Parser::prefix_parse_fn(&self.cur_token) {
            Some(prefix) => prefix(self),
            _ => {
                self.errors.push(format!(
                    "no prefix parse function for {:?} found",
                    self.cur_token
                ));
                None
            }
        };

        if let Some(mut left) = opt_left {
            while let Some(peek_token) = self.lexer.peek() {
                if *peek_token != Token::SEMICOLON && precedence < Precedence::from(peek_token) {
                    let infix = match Parser::infix_parse_fn(&peek_token) {
                        Some(infix) => infix,
                        _ => break,
                    };

                    self.next_token();

                    let expr = infix(self, left);
                    if expr.is_none() {
                        return None;
                    }

                    left = expr.unwrap();
                } else {
                    break;
                }
            }
            Some(left)
        } else {
            None
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        if !self.expect_peek(&Token::IDENT("".to_string())) {
            return None;
        }

        let ident = match &self.cur_token {
            Token::IDENT(ident) => ident.to_string(),
            _ => return None,
        };

        if !self.expect_peek(&Token::ASSIGN) {
            return None;
        }

        self.next_token();

        if let Some(expr) = self.parse_expression(Precedence::LOWEST) {
            if let Some(&Token::SEMICOLON) = self.lexer.peek() {
                self.next_token();
            }

            Some(Statement::Let(ident, expr))
        } else {
            None
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        if let Some(expr) = self.parse_expression(Precedence::LOWEST) {
            if let Some(&Token::SEMICOLON) = self.lexer.peek() {
                self.next_token();
            }

            Some(Statement::Return(expr))
        } else {
            None
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = match self.parse_expression(Precedence::LOWEST) {
            Some(expr) => expr,
            _ => return None,
        };

        let stmt = Statement::Expression(expr);

        match self.lexer.peek() {
            Some(&Token::SEMICOLON) => self.next_token(),
            _ => {}
        };

        Some(stmt)
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut block = BlockStatement::new();

        self.next_token();

        while self.cur_token != Token::RBRACE && self.cur_token != Token::EOF {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                block.statements.push(stmt);
            }
            self.next_token();
        }

        block
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<String>> {
        let mut identifiers = vec![];

        if let Some(&Token::RPAREN) = self.lexer.peek() {
            self.next_token();
            return Some(identifiers);
        }

        self.next_token();
        match &self.cur_token {
            Token::IDENT(ident) => identifiers.push(ident.to_string()),
            _ => return None,
        }

        while let Some(token) = self.lexer.peek() {
            if *token != Token::COMMA {
                break;
            }
            self.next_token();
            self.next_token();
            match &self.cur_token {
                Token::IDENT(ident) => identifiers.push(ident.to_string()),
                _ => return None,
            }
        }

        if !self.expect_peek(&Token::RPAREN) {
            return None;
        }

        Some(identifiers)
    }
}

#[cfg(test)]
mod parser_test {
    use super::*;
    use std::collections::HashMap;

    fn check_parser_errors(p: &Parser) {
        let errors = p.errors();
        if errors.len() == 0 {
            return;
        }

        println!("parser has {} errors", errors.len());
        for error in errors {
            println!("parser error: {}", error);
        }
        assert_eq!(errors.len(), 0);
    }

    fn is_statement_let(stmt: &Statement, name: &str) -> bool {
        match stmt {
            Statement::Let(ident, _expr) => {
                assert_eq!(ident, name);
                true
            }
            _ => false,
        }
    }

    fn is_expression_integer(expr: &Expression, value: isize) -> bool {
        match expr {
            Expression::Integer(integer) => {
                assert_eq!(*integer, value);
                true
            }
            _ => false,
        }
    }

    fn is_expression_identifier(expr: &Expression, value: &str) -> bool {
        match expr {
            Expression::Identifier(ident) => {
                assert_eq!(ident, value);
                true
            }
            _ => false,
        }
    }

    fn is_expression_boolean(expr: &Expression, value: bool) -> bool {
        match expr {
            Expression::Boolean(boolean) => {
                assert_eq!(*boolean, value);
                true
            }
            _ => false,
        }
    }

    fn is_expression_infix(
        expr: &Expression,
        left_expected: &Literal,
        infix_expected: &Infix,
        right_expected: &Literal,
    ) -> bool {
        match expr {
            Expression::Infix(left, infix, right) => {
                if !is_expression_literal(left, left_expected) {
                    return false;
                }
                assert_eq!(*infix, *infix_expected);

                if !is_expression_literal(right, right_expected) {
                    return false;
                }

                true
            }
            _ => false,
        }
    }

    enum Literal {
        Integer(isize),
        Identifier(String),
        Boolean(bool),
    }

    fn is_expression_literal(expr: &Expression, expected: &Literal) -> bool {
        match expected {
            Literal::Integer(integer) => is_expression_integer(expr, *integer),
            Literal::Identifier(identifier) => is_expression_identifier(expr, identifier),
            Literal::Boolean(boolean) => is_expression_boolean(expr, *boolean),
        }
    }

    #[test]
    fn test_statement_let() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383";

        let tests = vec!["x", "y", "foobar"];

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 3);

        for (tt, stmt) in tests.iter().zip(program.statements.iter()) {
            assert_eq!(is_statement_let(stmt, tt), true);
        }
    }

    #[test]
    fn test_statement_lets() {
        let tests = vec![
            ("let x = 5;", "x", Literal::Integer(5)),
            ("let y = true;", "y", Literal::Boolean(true)),
            (
                "let foobar = y",
                "foobar",
                Literal::Identifier("y".to_string()),
            ),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parser_errors(&p);
            assert_eq!(program.statements.len(), 1);

            let stmt = &program.statements[0];
            match stmt {
                Statement::Let(ident, expr) => {
                    assert_eq!(ident, tt.1);
                    assert_eq!(is_expression_literal(expr, &tt.2), true);
                }
                _ => assert!(false, "Statement is not Let"),
            }
        }
    }

    #[test]
    fn test_statement_let_errors() {
        let input = "let x 5;
let  = 10;
let  838383";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        //check_parser_errors(&p);
        assert_eq!(program.statements.len(), 3);
    }

    #[test]
    fn test_statement_return() {
        let input = "return 5;
return 10;
return 993322;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 3);

        for stmt in &program.statements {
            match stmt {
                Statement::Return(_) => {}
                _ => assert!(false, "Statement is not Return"),
            };
        }
    }

    #[test]
    fn test_expression_identifier() {
        let input = "foobar;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        for stmt in &program.statements {
            match stmt {
                Statement::Expression(expr) => match expr {
                    Expression::Identifier(ident) => assert_eq!(ident, "foobar"),
                    _ => assert!(false, "Expression is not Ident"),
                },
                _ => assert!(false, "Statement is not Expression"),
            };
        }
    }

    #[test]
    fn test_expression_integer() {
        let input = "5;";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        for stmt in &program.statements {
            match stmt {
                Statement::Expression(expr) => match expr {
                    Expression::Integer(integer) => assert_eq!(*integer, 5),
                    _ => assert!(false, "Expression is not Int"),
                },
                _ => assert!(false, "Statement is not Expression"),
            };
        }
    }

    #[test]
    fn test_expression_string() {
        let input = "\"hello world\";";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        for stmt in &program.statements {
            match stmt {
                Statement::Expression(expr) => match expr {
                    Expression::String(string) => assert_eq!(string, "hello world"),
                    _ => assert!(false, "Expression is not String"),
                },
                _ => assert!(false, "Statement is not Expression"),
            };
        }
    }

    #[test]
    fn test_expression_hash() {
        let input = "{\"one\": 1, \"two\":2, \"three\":3}";
        let expected: HashMap<String, isize> = [
            ("one".to_string(), 1),
            ("two".to_string(), 2),
            ("three".to_string(), 3),
        ]
        .iter()
        .cloned()
        .collect();

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        for stmt in &program.statements {
            match stmt {
                Statement::Expression(expr) => match expr {
                    Expression::Hash(hash) => {
                        assert_eq!(hash.pairs.len(), 3);
                        for (key, value) in &hash.pairs {
                            match key {
                                Expression::String(string) => {
                                    if let Some(expected_value) = expected.get(string) {
                                        assert_eq!(
                                            is_expression_integer(value, *expected_value),
                                            true
                                        );
                                    } else {
                                        assert!(false, "{} is not expected", string);
                                    }
                                }
                                _ => assert!(false, "{} is not String", *key),
                            };
                        }
                    }
                    _ => assert!(false, "Expression is not Hash"),
                },
                _ => assert!(false, "Statement is not Expression"),
            };
        }
    }

    #[test]
    fn test_expression_array() {
        let input = "[1, 2*2, 3+3]";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        for stmt in &program.statements {
            match stmt {
                Statement::Expression(expr) => match expr {
                    Expression::Array(array) => {
                        assert_eq!(array.len(), 3);
                        assert_eq!(is_expression_literal(&array[0], &Literal::Integer(1)), true);
                        assert_eq!(
                            is_expression_infix(
                                &array[1],
                                &Literal::Integer(2),
                                &Infix::ASTERISK,
                                &Literal::Integer(2)
                            ),
                            true
                        );
                        assert_eq!(
                            is_expression_infix(
                                &array[2],
                                &Literal::Integer(3),
                                &Infix::PLUS,
                                &Literal::Integer(3)
                            ),
                            true
                        );
                    }
                    _ => assert!(false, "Expression is not Array"),
                },
                _ => assert!(false, "Statement is not Expression"),
            };
        }
    }

    #[test]
    fn test_expression_if() {
        let input = "if (x<y) {x}";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        for stmt in &program.statements {
            match stmt {
                Statement::Expression(expr) => match expr {
                    Expression::If(condition, consequence, alternative) => {
                        assert_eq!(
                            is_expression_infix(
                                condition,
                                &Literal::Identifier("x".to_string()),
                                &Infix::LT,
                                &Literal::Identifier("y".to_string())
                            ),
                            true
                        );
                        assert_eq!(consequence.statements.len(), 1);
                        if let Some(stmt) = consequence.statements.first() {
                            match stmt {
                                Statement::Expression(expr) => assert_eq!(
                                    is_expression_literal(
                                        expr,
                                        &Literal::Identifier("x".to_string())
                                    ),
                                    true
                                ),
                                _ => assert!(false, "Statement is not Expression"),
                            };

                            assert!(alternative.is_none(), "alternative should be none");
                        } else {
                            assert!(false, "no statement in consequence");
                        }
                    }
                    _ => assert!(false, "Expression is not If"),
                },
                _ => assert!(false, "Statement is not Expression"),
            };
        }
    }

    #[test]
    fn test_expression_ifelse() {
        let input = "if (x<y) {x} else {y}";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        for stmt in &program.statements {
            match stmt {
                Statement::Expression(expr) => match expr {
                    Expression::If(condition, consequence, alternative) => {
                        assert_eq!(
                            is_expression_infix(
                                condition,
                                &Literal::Identifier("x".to_string()),
                                &Infix::LT,
                                &Literal::Identifier("y".to_string())
                            ),
                            true
                        );
                        assert_eq!(consequence.statements.len(), 1);
                        if let Some(stmt) = consequence.statements.first() {
                            match stmt {
                                Statement::Expression(expr) => assert_eq!(
                                    is_expression_literal(
                                        expr,
                                        &Literal::Identifier("x".to_string())
                                    ),
                                    true
                                ),
                                _ => assert!(false, "Statement is not Expression"),
                            };

                            if let Some(alternative) = alternative {
                                if let Some(stmt) = alternative.statements.first() {
                                    match stmt {
                                        Statement::Expression(expr) => assert_eq!(
                                            is_expression_literal(
                                                expr,
                                                &Literal::Identifier("y".to_string())
                                            ),
                                            true
                                        ),
                                        _ => assert!(false, "Statement is not Expression"),
                                    };
                                } else {
                                    assert!(false, "no statement in alternative");
                                }
                            } else {
                                assert!(false, "alternative should be not none");
                            }
                        } else {
                            assert!(false, "no statement in consequence");
                        }
                    }
                    _ => assert!(false, "Expression is not If"),
                },
                _ => assert!(false, "Statement is not Expression"),
            };
        }
    }

    #[test]
    fn test_expression_function() {
        let input = "fn(x,y) {x+y;}";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        for stmt in &program.statements {
            match stmt {
                Statement::Expression(expr) => match expr {
                    Expression::Function(parameters, body) => {
                        assert_eq!(parameters.len(), 2);
                        assert_eq!(parameters[0], "x");
                        assert_eq!(parameters[1], "y");
                        assert_eq!(body.statements.len(), 1);
                        if let Some(stmt) = body.statements.first() {
                            match stmt {
                                Statement::Expression(expr) => assert_eq!(
                                    is_expression_infix(
                                        expr,
                                        &Literal::Identifier("x".to_string()),
                                        &Infix::PLUS,
                                        &Literal::Identifier("y".to_string())
                                    ),
                                    true
                                ),
                                _ => assert!(false, "Statement is not Expression"),
                            };
                        } else {
                            assert!(false, "no statement in body");
                        }
                    }
                    _ => assert!(false, "Expression is not Function"),
                },
                _ => assert!(false, "Statement is not Expression"),
            };
        }
    }

    #[test]
    fn test_expression_function_parameters() {
        let tests = vec![
            ("fn() {};", vec![]),
            ("fn(x) {};", vec!["x"]),
            ("fn(x,y,z) {x+y;}", vec!["x", "y", "z"]),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parser_errors(&p);

            for stmt in &program.statements {
                match stmt {
                    Statement::Expression(expr) => match expr {
                        Expression::Function(parameters, _body) => {
                            assert_eq!(parameters.len(), tt.1.len());
                            for (t, p) in tt.1.iter().zip(parameters.iter()) {
                                assert_eq!(*t, *p);
                            }
                        }
                        _ => assert!(false, "Expression is not Function"),
                    },
                    _ => assert!(false, "Statement is not Expression"),
                };
            }
        }
    }

    #[test]
    fn test_expression_call() {
        let input = "add(1, 2*3, 4+5);";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(&p);
        assert_eq!(program.statements.len(), 1);

        for stmt in &program.statements {
            match stmt {
                Statement::Expression(expr) => match expr {
                    Expression::Call(function, arguments) => {
                        assert_eq!(
                            is_expression_literal(
                                function,
                                &Literal::Identifier("add".to_string())
                            ),
                            true
                        );

                        assert_eq!(arguments.len(), 3);

                        assert_eq!(
                            is_expression_literal(&arguments[0], &Literal::Integer(1),),
                            true
                        );
                        assert_eq!(
                            is_expression_infix(
                                &arguments[1],
                                &Literal::Integer(2),
                                &Infix::ASTERISK,
                                &Literal::Integer(3)
                            ),
                            true
                        );
                        assert_eq!(
                            is_expression_infix(
                                &arguments[2],
                                &Literal::Integer(4),
                                &Infix::PLUS,
                                &Literal::Integer(5)
                            ),
                            true
                        );
                    }
                    _ => assert!(false, "Expression is not Function"),
                },
                _ => assert!(false, "Statement is not Expression"),
            };
        }
    }

    #[test]
    fn test_parsing_prefix_expressions() {
        let prefix_tests = vec![
            ("!5;", "!", Literal::Integer(5)),
            ("-15", "-", Literal::Integer(15)),
            ("!true", "!", Literal::Boolean(true)),
            ("!false", "!", Literal::Boolean(false)),
        ];

        for tt in prefix_tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parser_errors(&p);
            assert_eq!(program.statements.len(), 1);

            for stmt in &program.statements {
                match stmt {
                    Statement::Expression(expr) => match expr {
                        Expression::Prefix(prefix, right) => {
                            assert_eq!(prefix.to_string(), tt.1);
                            assert_eq!(is_expression_literal(right, &tt.2), true);
                        }
                        _ => assert!(false, "Expression is not Prefix"),
                    },
                    _ => assert!(false, "Statement is not Expression"),
                };
            }
        }
    }

    #[test]
    fn test_parsing_infix_expressions() {
        let infix_tests = vec![
            ("5+5;", Literal::Integer(5), "+", Literal::Integer(5)),
            ("5-5;", Literal::Integer(5), "-", Literal::Integer(5)),
            ("5*5;", Literal::Integer(5), "*", Literal::Integer(5)),
            ("5/5;", Literal::Integer(5), "/", Literal::Integer(5)),
            ("5>5;", Literal::Integer(5), ">", Literal::Integer(5)),
            ("5<5;", Literal::Integer(5), "<", Literal::Integer(5)),
            ("5==5;", Literal::Integer(5), "==", Literal::Integer(5)),
            ("5!=5;", Literal::Integer(5), "!=", Literal::Integer(5)),
            (
                "true==true",
                Literal::Boolean(true),
                "==",
                Literal::Boolean(true),
            ),
            (
                "true!=false",
                Literal::Boolean(true),
                "!=",
                Literal::Boolean(false),
            ),
            (
                "false==false",
                Literal::Boolean(false),
                "==",
                Literal::Boolean(false),
            ),
        ];

        for tt in infix_tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parser_errors(&p);
            assert_eq!(program.statements.len(), 1);

            for stmt in &program.statements {
                match stmt {
                    Statement::Expression(expr) => match expr {
                        Expression::Infix(left, infix, right) => {
                            assert_eq!(is_expression_literal(left, &tt.1), true);
                            assert_eq!(infix.to_string(), tt.2);
                            assert_eq!(is_expression_literal(right, &tt.3), true);
                        }
                        _ => assert!(false, "Expression is not Infix"),
                    },
                    _ => assert!(false, "Statement is not Expression"),
                };
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            ("-a*b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a+b+c", "((a + b) + c)"),
            ("a+b-c", "((a + b) - c)"),
            ("a*b*c", "((a * b) * c)"),
            ("a*b/c", "((a * b) / c)"),
            ("a+b/c", "(a + (b / c))"),
            ("a+b*c+d/e-f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3+4; -5*5", "(3 + 4)\n((-5) * 5)"),
            ("5>4==3<4", "((5 > 4) == (3 < 4))"),
            ("5>4!=3<4", "((5 > 4) != (3 < 4))"),
            ("3+4*5==3*1+4*5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
            ("true", "true"),
            ("false", "false"),
            ("3>5==false", "((3 > 5) == false)"),
            ("3<5==true", "((3 < 5) == true)"),
            ("1+(2+3)+4", "((1 + (2 + 3)) + 4)"),
            ("(5+5)*2", "((5 + 5) * 2)"),
            ("2/(5+5)", "(2 / (5 + 5))"),
            ("-(5+5)", "(-(5 + 5))"),
            ("!(true==true)", "(!(true == true))"),
            ("a + add(b *c) +d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2*3, 4 +5, add(6, 7*8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d/f+g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            ("a*[1,2,3,4][b*c]*d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
            (
                "add(a*b[2], b[1], 2*[1,2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            check_parser_errors(&p);

            assert_eq!(program.to_string(), tt.1);
        }
    }
}
