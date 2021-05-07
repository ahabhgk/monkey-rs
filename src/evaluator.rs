use crate::{
    ast::{Expression, HashLiteral, Infix, Prefix, Program, Statement},
    environment::Environment,
    object::{Builtin, Function, HashObject, Object},
};
use std::{cell::RefCell, collections::HashMap, fmt, rc::Rc};

#[derive(Debug)]
pub struct EvalError {
    pub message: String,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for EvalError {
    fn description(&self) -> &str {
        &self.message
    }
}

pub struct Evaluator;

impl Evaluator {
    pub fn eval(program: &Program, env: Rc<RefCell<Environment>>) -> Result<Rc<Object>, EvalError> {
        Self::eval_statements(&program.statements, env)
    }

    fn eval_statements(
        stmts: &[Statement],
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<Object>, EvalError> {
        let mut result = Rc::new(Object::Null);

        for stmt in stmts {
            result = Self::eval_statement(stmt, Rc::clone(&env))?;

            match &*result {
                Object::Return(obj) => return Ok(Rc::clone(obj)),
                _ => {}
            };
        }

        Ok(result)
    }

    fn eval_block_statements(
        stmts: &[Statement],
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<Object>, EvalError> {
        let mut result = Rc::new(Object::Null);

        for stmt in stmts {
            result = Self::eval_statement(stmt, Rc::clone(&env))?;

            match &*result {
                Object::Return(_) => return Ok(result),
                _ => {}
            };
        }

        Ok(result)
    }

    fn eval_statement(
        stmt: &Statement,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<Object>, EvalError> {
        match stmt {
            Statement::Let(ident, expr) => {
                let obj = Self::eval_expression(expr, Rc::clone(&env))?;
                env.borrow_mut().set(ident.clone(), Rc::clone(&obj));
                Ok(obj)
            }
            Statement::Return(expr) => {
                let obj = Self::eval_expression(expr, env)?;
                Ok(Rc::new(Object::Return(obj)))
            }
            Statement::Expression(expr) => Self::eval_expression(expr, env),
        }
    }

    fn eval_expression(
        expr: &Expression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<Object>, EvalError> {
        match expr {
            Expression::Identifier(ident) => {
                if let Some(obj) = env.borrow().get(&ident) {
                    return Ok(obj);
                }

                if let Some(obj) = Builtin::lookup(&ident) {
                    return Ok(obj);
                }

                Err(EvalError {
                    message: format!("identifier not found: {}", ident),
                })
            }
            Expression::Integer(integer) => Ok(Rc::new(Object::Integer(*integer))),
            Expression::Boolean(boolean) => Ok(Rc::new(Object::Boolean(*boolean))),
            Expression::String(string) => Ok(Rc::new(Object::String(string.to_string()))),
            Expression::Prefix(prefix, right) => {
                let right = Self::eval_expression(right, env)?;
                Self::eval_prefix_expression(prefix, right)
            }
            Expression::Infix(left, infix, right) => {
                let left = Self::eval_expression(left, Rc::clone(&env))?;
                let right = Self::eval_expression(right, env)?;
                Self::eval_infix_expression(left, infix, right)
            }
            Expression::If(condition, consequence, alternative) => {
                let condition = Self::eval_expression(condition, Rc::clone(&env))?;
                if Self::is_truthy(&condition) {
                    Self::eval_block_statements(&consequence.statements, env)
                } else if let Some(alternative) = alternative {
                    Self::eval_block_statements(&alternative.statements, env)
                } else {
                    Ok(Rc::new(Object::Null))
                }
            }
            Expression::Function(parameters, body) => Ok(Rc::new(Object::Function(Function {
                parameters: parameters.clone(),
                body: body.clone(),
                env,
            }))),
            Expression::Call(function, arguments) => {
                let function = Self::eval_expression(function, Rc::clone(&env))?;
                let args = Self::eval_expressions(arguments, env)?;
                Self::apply_function(function, args)
            }
            Expression::Array(array) => {
                let elements = Self::eval_expressions(array, env)?;
                Ok(Rc::new(Object::Array(elements)))
            }
            Expression::Index(left, right) => {
                let array = Self::eval_expression(left, Rc::clone(&env))?;
                let index = Self::eval_expression(right, env)?;
                Self::eval_index_expression(array, index)
            }
            Expression::Hash(hash) => Self::eval_hash_expression(hash, env),
            //_ => Err(EvalError {
            //    message: format!("to be implemented {}", expr),
            //}),
        }
    }

    fn eval_expressions(
        exprs: &[Expression],
        env: Rc<RefCell<Environment>>,
    ) -> Result<Vec<Rc<Object>>, EvalError> {
        let mut results = vec![];

        for expr in exprs {
            let evaluated = Self::eval_expression(expr, Rc::clone(&env))?;
            results.push(evaluated);
        }

        Ok(results)
    }

    fn eval_prefix_expression(prefix: &Prefix, right: Rc<Object>) -> Result<Rc<Object>, EvalError> {
        match prefix {
            &Prefix::BANG => Self::eval_bang_operator_expression(right),
            &Prefix::MINUS => Self::eval_minus_operator_expression(right),
        }
    }

    fn eval_bang_operator_expression(right: Rc<Object>) -> Result<Rc<Object>, EvalError> {
        match &*right {
            Object::Boolean(boolean) => Ok(Rc::new(Object::Boolean(!*boolean))),
            Object::Null => Ok(Rc::new(Object::Boolean(true))),
            _ => Ok(Rc::new(Object::Boolean(false))),
        }
    }

    fn eval_minus_operator_expression(right: Rc<Object>) -> Result<Rc<Object>, EvalError> {
        match &*right {
            Object::Integer(integer) => Ok(Rc::new(Object::Integer(-*integer))),
            _ => Err(EvalError {
                message: format!("illegal operator: -{}", right),
            }),
        }
    }

    fn eval_infix_expression(
        left: Rc<Object>,
        infix: &Infix,
        right: Rc<Object>,
    ) -> Result<Rc<Object>, EvalError> {
        match (&*left, infix, &*right) {
            (Object::Integer(left), _, Object::Integer(right)) => {
                Self::eval_integer_infix_expression(*left, infix, *right)
            }
            (Object::String(left), _, Object::String(right)) => {
                Self::eval_string_infix_expression(left, infix, right)
            }
            (_, &Infix::EQ, _) => Ok(Rc::new(Object::Boolean(*left == *right))),
            (_, &Infix::NEQ, _) => Ok(Rc::new(Object::Boolean(*left != *right))),
            _ => Err(EvalError {
                message: format!("illegal operator: {} {} {}", left, infix, right),
            }),
        }
    }

    fn eval_index_expression(
        array: Rc<Object>,
        index: Rc<Object>,
    ) -> Result<Rc<Object>, EvalError> {
        match (&*array, &*index) {
            (Object::Array(array), Object::Integer(index)) => {
                Self::eval_array_index_expression(array, *index)
            }
            (Object::Hash(hash), _) => Self::eval_hash_index_expression(hash, index),
            _ => Err(EvalError {
                message: format!("index operator not supported: {}", array),
            }),
        }
    }

    fn eval_hash_expression(
        hash: &HashLiteral,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<Object>, EvalError> {
        let mut pairs: HashMap<Rc<Object>, Rc<Object>> = HashMap::new();

        for (key, val) in &hash.pairs {
            let key = Self::eval_expression(key, Rc::clone(&env))?;
            let val = Self::eval_expression(val, Rc::clone(&env))?;
            pairs.insert(key, val);
        }

        Ok(Rc::new(Object::Hash(HashObject { pairs })))
    }

    fn eval_integer_infix_expression(
        left: isize,
        infix: &Infix,
        right: isize,
    ) -> Result<Rc<Object>, EvalError> {
        let obj = match infix {
            &Infix::PLUS => Object::Integer(left + right),
            &Infix::MINUS => Object::Integer(left - right),
            &Infix::ASTERISK => Object::Integer(left * right),
            &Infix::SLASH => Object::Integer(left / right),
            &Infix::LT => Object::Boolean(left < right),
            &Infix::GT => Object::Boolean(left > right),
            &Infix::EQ => Object::Boolean(left == right),
            &Infix::NEQ => Object::Boolean(left != right),
        };

        Ok(Rc::new(obj))
    }

    fn eval_string_infix_expression(
        left: &str,
        infix: &Infix,
        right: &str,
    ) -> Result<Rc<Object>, EvalError> {
        let obj = match infix {
            &Infix::PLUS => Object::String(left.to_string() + right),
            _ => {
                return Err(EvalError {
                    message: format!("illegal operator: {} {} {}", left, infix, right),
                })
            }
        };

        Ok(Rc::new(obj))
    }

    fn eval_array_index_expression(
        array: &[Rc<Object>],
        index: isize,
    ) -> Result<Rc<Object>, EvalError> {
        if index < 0 || index >= array.len() as isize {
            Ok(Rc::new(Object::Null))
        } else {
            Ok(Rc::clone(&array[index as usize]))
        }
    }

    fn eval_hash_index_expression(
        hash: &HashObject,
        index: Rc<Object>,
    ) -> Result<Rc<Object>, EvalError> {
        match &*index {
            Object::String(_) | Object::Boolean(_) | Object::Integer(_) => {
                if let Some(val) = hash.pairs.get(&*index) {
                    Ok(Rc::clone(val))
                } else {
                    Ok(Rc::new(Object::Null))
                }
            }
            _ => Err(EvalError {
                message: format!("unusable as hash key: {}", index),
            }),
        }
    }

    fn apply_function(
        function: Rc<Object>,
        args: Vec<Rc<Object>>,
    ) -> Result<Rc<Object>, EvalError> {
        match &*function {
            Object::Function(function) => {
                let extended_env = Environment::from(Rc::clone(&function.env));
                for (param, arg) in function.parameters.iter().zip(args.iter()) {
                    extended_env.borrow_mut().set(param.clone(), arg.clone());
                }
                let evaluated = Self::eval(&function.body, extended_env)?;
                match &*evaluated {
                    Object::Return(obj) => Ok(Rc::clone(obj)),
                    _ => Ok(evaluated),
                }
            }
            Object::Builtin(builtin) => builtin.apply(&args),
            _ => Err(EvalError {
                message: format!("not a function: {}", function),
            }),
        }
    }

    fn is_truthy(obj: &Object) -> bool {
        match obj {
            Object::Null => false,
            Object::Boolean(boolean) => *boolean,
            _ => true,
        }
    }
}

#[cfg(test)]
mod evaluator {
    use super::*;
    use crate::lexer::*;
    use crate::parser::*;

    fn is_object_integer(obj: &Object, test: &str, expected: isize) -> bool {
        match obj {
            Object::Integer(integer) => {
                assert!(
                    *integer == expected,
                    "Eval {}, expected Integer({}), but got {}",
                    test,
                    expected,
                    obj
                );
                true
            }
            _ => {
                assert!(
                    false,
                    "Eval {}, expected Integer({}), but got {}",
                    test, expected, obj
                );
                false
            }
        }
    }

    fn is_object_boolean(obj: &Object, test: &str, expected: bool) -> bool {
        match obj {
            Object::Boolean(boolean) => {
                assert!(
                    *boolean == expected,
                    "Eval {}, expected Boolean({}), but got {}",
                    test,
                    expected,
                    obj
                );
                true
            }
            _ => {
                assert!(
                    false,
                    "Eval {}, expected Boolean({}), but got {}",
                    test, expected, obj
                );
                false
            }
        }
    }

    fn is_object_string(obj: &Object, test: &str, expected: &str) -> bool {
        match obj {
            Object::String(string) => {
                assert!(
                    string == expected,
                    "Eval {}, expected String({}), but got {}",
                    test,
                    expected,
                    obj
                );
                true
            }
            _ => {
                assert!(
                    false,
                    "Eval {}, expected String({}), but got {}",
                    test, expected, obj
                );
                false
            }
        }
    }

    #[test]
    fn test_eval_expression_integer() -> Result<(), EvalError> {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5+5+5+5-10", 10),
            ("2*2*2*2*2", 32),
            ("-50+100+-50", 0),
            ("5*2+10", 20),
            ("20+2*-10", 0),
            ("2*(5+10)", 30),
            ("3*3*3+10", 37),
            ("3*(3*3)+10", 37),
            ("(5+10*2+15/3)*2+-10", 50),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            let evaluated = Evaluator::eval(&program, env)?;
            assert_eq!(is_object_integer(&*evaluated, tt.0, tt.1), true);
        }
        Ok(())
    }

    #[test]
    fn test_eval_expression_boolean() -> Result<(), EvalError> {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1<2", true),
            ("1>2", false),
            ("1<1", false),
            ("1>1", false),
            ("1==1", true),
            ("1!=1", false),
            ("1==2", false),
            ("1!=2", true),
            ("true==true", true),
            ("false==false", true),
            ("true==false", false),
            ("true!=false", true),
            ("false!=true", true),
            ("(1<2)==true", true),
            ("(1<2)==false", false),
            ("(1>2)==true", false),
            ("(1>2)==false", true),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            let evaluated = Evaluator::eval(&program, env)?;
            assert_eq!(is_object_boolean(&*evaluated, tt.0, tt.1), true);
        }
        Ok(())
    }

    #[test]
    fn test_eval_expression_string() -> Result<(), EvalError> {
        let tests = vec![("\"Hello World\"", "Hello World")];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            let evaluated = Evaluator::eval(&program, env)?;
            assert_eq!(is_object_string(&*evaluated, tt.0, tt.1), true);
        }
        Ok(())
    }

    #[test]
    fn test_eval_expression_string_concatenation() -> Result<(), EvalError> {
        let tests = vec![("\"Hello\" + \" \" + \"World\"", "Hello World")];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            let evaluated = Evaluator::eval(&program, env)?;
            assert_eq!(is_object_string(&*evaluated, tt.0, tt.1), true);
        }
        Ok(())
    }

    #[test]
    fn test_eval_expression_array() -> Result<(), EvalError> {
        let input = "[1, 2*2, 3+3]";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = Evaluator::eval(&program, env)?;
        match &*evaluated {
            Object::Array(array) => {
                assert_eq!(array.len(), 3);
                assert_eq!(is_object_integer(&*array[0], input, 1), true);
                assert_eq!(is_object_integer(&*array[1], input, 4), true);
                assert_eq!(is_object_integer(&*array[2], input, 6), true);
            }
            _ => assert!(false, "Object is not Array"),
        };

        Ok(())
    }

    #[test]
    fn test_eval_expression_array_index() -> Result<(), EvalError> {
        let tests = vec![
            ("[1, 2, 3][0]", Some(1)),
            ("[1, 2, 3][1]", Some(2)),
            ("[1, 2, 3][2]", Some(3)),
            ("let i = 0; [1][i]", Some(1)),
            ("[1, 2, 3][1+1]", Some(3)),
            ("let myArray = [1,2,3]; myArray[2];", Some(3)),
            (
                "let myArray = [1,2,3]; myArray[0]+myArray[1]+myArray[2];",
                Some(6),
            ),
            (
                "let myArray = [1,2,3]; let i = myArray[0]; myArray[i]",
                Some(2),
            ),
            ("[1,2,3][3]", None),
            ("[1,2,3][-1]", None),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            let evaluated = Evaluator::eval(&program, env)?;
            match &*evaluated {
                Object::Null => assert_eq!(tt.1, None),
                _ => {
                    if let Some(result) = tt.1 {
                        assert_eq!(is_object_integer(&*evaluated, tt.0, result), true);
                    } else {
                        assert!(false, "Wrong result: expected null, but got {}", evaluated);
                    }
                }
            }
        }

        Ok(())
    }

    #[test]
    fn test_eval_operator_bang() -> Result<(), EvalError> {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            let evaluated = Evaluator::eval(&program, env)?;
            assert_eq!(is_object_boolean(&*evaluated, tt.0, tt.1), true);
        }
        Ok(())
    }

    #[test]
    fn test_eval_expression_ifelse() -> Result<(), EvalError> {
        let tests = vec![
            ("if(true) {10}", Some(10)),
            ("if(false) {10}", None),
            ("if(1) {10}", Some(10)),
            ("if(1<2) {10}", Some(10)),
            ("if(1>2) {10}", None),
            ("if(1>2) {10} else {20}", Some(20)),
            ("if(1<2) {10} else {20}", Some(10)),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            let evaluated = Evaluator::eval(&program, env)?;
            if let Some(expected) = tt.1 {
                assert_eq!(is_object_integer(&*evaluated, tt.0, expected), true);
            } else {
                assert_eq!(&*evaluated, &Object::Null);
            }
        }
        Ok(())
    }

    #[test]
    fn test_eval_statement_return() -> Result<(), EvalError> {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2*5; 9;", 10),
            ("9; return 2*5; 9;", 10),
            ("if(10>1) { if(10>1) {return 10;} return 1; }", 10),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            let evaluated = Evaluator::eval(&program, env)?;
            assert_eq!(is_object_integer(&*evaluated, tt.0, tt.1), true);
        }
        Ok(())
    }

    #[test]
    fn test_error_handling() -> Result<(), EvalError> {
        let tests = vec![
            ("-true", "illegal operator: -true"),
            ("5+true;", "illegal operator: 5 + true"),
            ("5+true; 5;", "illegal operator: 5 + true"),
            ("true + false;", "illegal operator: true + false"),
            ("5; true+false; 5}", "illegal operator: true + false"),
            ("if (10>1) {true+false;}", "illegal operator: true + false"),
            (
                "if(10>1) { if(10>1) {return true+false;} return 1; }",
                "illegal operator: true + false",
            ),
            ("\"Hello\" - \"World\"", "illegal operator: Hello - World"),
            (
                "{\"name\": \"Monkey\"}[fn(x) {x}];",
                "unusable as hash key: fn(x) {\nx\n}",
            ),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            let evaluated = Evaluator::eval(&program, env);
            match evaluated {
                Err(msg) => assert!(
                    &msg.message == tt.1,
                    "wrong error msg in {}, expected={}, got={}",
                    tt.0,
                    tt.1,
                    msg
                ),
                Ok(evaluated) => assert!(
                    false,
                    "wrong error msg in {}, expected={}, got={}",
                    tt.0, tt.1, evaluated
                ),
            };
        }
        Ok(())
    }

    #[test]
    fn test_let_statements() -> Result<(), EvalError> {
        let tests = vec![
            ("let a=5; a;", 5),
            ("let a=5*5; a;", 25),
            ("let a=5; let b=a; b;", 5),
            ("let a=5; let b=a; let c=a+b+5; c;", 15),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            let evaluated = Evaluator::eval(&program, env)?;
            assert_eq!(is_object_integer(&*evaluated, tt.0, tt.1), true);
        }
        Ok(())
    }

    #[test]
    fn test_function_object() -> Result<(), EvalError> {
        let input = "fn(x) { x+2; };";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = Evaluator::eval(&program, env)?;
        match &*evaluated {
            Object::Function(function) => {
                assert_eq!(function.parameters.len(), 1);
                assert_eq!(function.parameters[0], "x");
                assert_eq!(function.body.to_string(), "(x + 2)")
            }
            _ => assert!(false, "object is not Function. got {}", evaluated),
        };
        Ok(())
    }

    #[test]
    fn test_function_application() -> Result<(), EvalError> {
        let tests = vec![
            ("let identity = fn(x) {x;}; identity(5);", 5),
            ("let identity = fn(x) {return x;}; identity(5);", 5),
            ("let double = fn(x) {x*2;}; double(5);", 10),
            ("let add = fn(x,y) {x+y;}; add(5,5);", 10),
            ("let add = fn(x,y) {x+y;}; add(5+5, add(5,5));", 20),
            ("fn(x){x;}(5)", 5),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            let evaluated = Evaluator::eval(&program, env)?;
            assert_eq!(is_object_integer(&*evaluated, tt.0, tt.1), true);
        }
        Ok(())
    }

    #[test]
    fn test_builtin_functions() -> Result<(), EvalError> {
        let tests = vec![
            ("len(\"\")", 0),
            ("len(\"four\")", 4),
            ("len(\"hello world\")", 11),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            let evaluated = Evaluator::eval(&program, env)?;
            assert_eq!(is_object_integer(&*evaluated, tt.0, tt.1), true);
        }
        Ok(())
    }

    #[test]
    fn test_illegal_builtin_functions() -> Result<(), EvalError> {
        let tests = vec![
            ("len(1)", "argument to \"len\" not supported, got 1"),
            (
                "len(\"one\", \"two\")",
                "wrong number of arguments. got=2, want=1",
            ),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            match Evaluator::eval(&program, env) {
                Err(error) => assert_eq!(error.message, tt.1),
                _ => {}
            }
        }
        Ok(())
    }

    #[test]
    fn test_hash_object() -> Result<(), EvalError> {
        let input = "let two = \"two\";
        {
            \"one\": 10 - 9,
            two: 1+1,
            4:4,
            true: 5,
            false: 6
        }
    ";

        let l = Lexer::new(input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        let env = Environment::new();
        let evaluated = Evaluator::eval(&program, env)?;
        match &*evaluated {
            Object::Hash(hash_object) => {
                assert_eq!(hash_object.pairs.len(), 5);
                for (key, val) in hash_object.pairs.iter() {
                    match &**key {
                        Object::String(string) => {
                            match string.as_str() {
                                "one" => assert_eq!(is_object_integer(val, input, 1), true),
                                "two" => assert_eq!(is_object_integer(val, input, 2), true),
                                _ => assert!(false, "{} not found in {}", string, input),
                            };
                        }
                        Object::Integer(4) => assert_eq!(is_object_integer(val, input, 4), true),
                        Object::Boolean(true) => assert_eq!(is_object_integer(val, input, 5), true),
                        Object::Boolean(false) => {
                            assert_eq!(is_object_integer(val, input, 6), true)
                        }
                        _ => assert!(false, "{}: {} not found in {}", key, val, input),
                    };
                }
            }
            _ => assert!(false, "object is not Hash. got {}", evaluated),
        };
        Ok(())
    }

    #[test]
    fn test_hash_index_expressions() -> Result<(), EvalError> {
        let tests = vec![
            ("{\"foo\": 5}[\"foo\"]", Some(5)),
            ("{\"foo\": 5}[\"bar\"]", None),
            ("let key = \"foo\"; {\"foo\": 5}[key]", Some(5)),
            ("{}[\"foo\"]", None),
            ("{5:5}[5]", Some(5)),
            ("{true:5}[true]", Some(5)),
            ("{false:5}[false]", Some(5)),
        ];

        for tt in tests {
            let l = Lexer::new(tt.0);
            let mut p = Parser::new(l);

            let program = p.parse_program();
            let env = Environment::new();
            let evaluated = Evaluator::eval(&program, env)?;
            if let Some(expected) = tt.1 {
                assert_eq!(is_object_integer(&*evaluated, tt.0, expected), true);
            } else {
                assert_eq!(&*evaluated, &Object::Null);
            }
        }
        Ok(())
    }
}
