use crate::{ast::BlockStatement, environment::Environment, evaluator::EvalError};
use std::{cell::RefCell, collections::HashMap, fmt, hash::Hash, hash::Hasher, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Object {
    Null,
    Integer(isize),
    Boolean(bool),
    String(String),
    Array(Vec<Rc<Object>>),
    Return(Rc<Object>),
    Function(Function),
    Builtin(Builtin),
    Hash(HashObject),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            Object::Null => "null".to_string(),
            Object::Integer(integer) => format!("{}", integer),
            Object::Boolean(boolean) => format!("{}", boolean),
            Object::String(string) => format!("{}", string),
            Object::Array(array) => {
                let elements: Vec<String> =
                    array.iter().map(|element| element.to_string()).collect();
                format!("[{}]", elements.join(", "))
            }
            Object::Return(object) => format!("{}", object),
            Object::Function(function) => format!("{}", function),
            Object::Builtin(builtin) => format!("{:?}", builtin),
            Object::Hash(hash) => format!("{}", hash),
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub parameters: Vec<String>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params: Vec<String> = self
            .parameters
            .iter()
            .map(|param| param.to_string())
            .collect();
        write!(f, "fn({}) {{\n{}\n}}", params.join(", "), self.body)
    }
}

impl Hash for Function {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("hash for function not supported");
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HashObject {
    pub pairs: HashMap<Rc<Object>, Rc<Object>>,
}

impl fmt::Display for HashObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pairs: Vec<String> = (&self.pairs)
            .iter()
            .map(|(key, value)| format!("{}: {}", key.to_string(), value.to_string()))
            .collect();
        write!(f, "{{{}}}", pairs.join(", "))
    }
}

impl Hash for HashObject {
    fn hash<H: Hasher>(&self, _state: &mut H) {
        panic!("hash for HashObject not supported");
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Builtin {
    Len,
    First,
    Last,
    Rest,
    Push,
    Print,
}

impl Builtin {
    pub fn lookup(name: &str) -> Option<Rc<Object>> {
        match name {
            "len" => Some(Rc::new(Object::Builtin(Builtin::Len))),
            "first" => Some(Rc::new(Object::Builtin(Builtin::First))),
            "last" => Some(Rc::new(Object::Builtin(Builtin::Last))),
            "rest" => Some(Rc::new(Object::Builtin(Builtin::Rest))),
            "push" => Some(Rc::new(Object::Builtin(Builtin::Push))),
            "print" => Some(Rc::new(Object::Builtin(Builtin::Print))),
            _ => None,
        }
    }

    pub fn apply(&self, args: &[Rc<Object>]) -> Result<Rc<Object>, EvalError> {
        match self {
            Builtin::Len => {
                if args.len() != 1 {
                    return Err(EvalError {
                        message: format!("wrong number of arguments. got={}, want=1", args.len()),
                    });
                }

                match &*args[0] {
                    Object::Array(array) => Ok(Rc::new(Object::Integer(array.len() as isize))),
                    Object::String(string) => Ok(Rc::new(Object::Integer(string.len() as isize))),
                    _ => Err(EvalError {
                        message: format!("argument to \"len\" not supported, got {}", args[0]),
                    }),
                }
            }
            Builtin::First => {
                if args.len() != 1 {
                    return Err(EvalError {
                        message: format!("wrong number of arguments. got={}, want=1", args.len()),
                    });
                }

                match &*args[0] {
                    Object::Array(array) => {
                        if array.len() > 0 {
                            Ok(Rc::clone(&array[0]))
                        } else {
                            Ok(Rc::new(Object::Null))
                        }
                    }
                    _ => Err(EvalError {
                        message: format!("argument to \"first\" not supported, got {}", args[0]),
                    }),
                }
            }
            Builtin::Last => {
                if args.len() != 1 {
                    return Err(EvalError {
                        message: format!("wrong number of arguments. got={}, want=1", args.len()),
                    });
                }

                match &*args[0] {
                    Object::Array(array) => {
                        if array.len() > 0 {
                            Ok(Rc::clone(&array[array.len() - 1]))
                        } else {
                            Ok(Rc::new(Object::Null))
                        }
                    }
                    _ => Err(EvalError {
                        message: format!("argument to \"last\" not supported, got {}", args[0]),
                    }),
                }
            }
            Builtin::Rest => {
                if args.len() != 1 {
                    return Err(EvalError {
                        message: format!("wrong number of arguments. got={}, want=1", args.len()),
                    });
                }

                match &*args[0] {
                    Object::Array(array) => {
                        if array.len() > 0 {
                            let mut rest = vec![];
                            for i in 1..array.len() {
                                rest.push(Rc::new((*array[i]).clone()));
                            }
                            Ok(Rc::new(Object::Array(rest)))
                        } else {
                            Ok(Rc::new(Object::Null))
                        }
                    }
                    _ => Err(EvalError {
                        message: format!("argument to \"rest\" not supported, got {}", args[0]),
                    }),
                }
            }
            Builtin::Push => {
                if args.len() != 2 {
                    return Err(EvalError {
                        message: format!("wrong number of arguments. got={}, want=2", args.len()),
                    });
                }

                match &*args[0] {
                    Object::Array(array) => {
                        if array.len() > 0 {
                            let mut push = vec![];
                            for i in 0..array.len() {
                                push.push(Rc::new((*array[i]).clone()));
                            }
                            push.push(Rc::new((*args[1]).clone()));
                            Ok(Rc::new(Object::Array(push)))
                        } else {
                            Ok(Rc::new(Object::Null))
                        }
                    }
                    _ => Err(EvalError {
                        message: format!("argument to \"push\" not supported, got {}", args[0]),
                    }),
                }
            }
            Builtin::Print => {
                for arg in args {
                    println!("{}", arg);
                }
                Ok(Rc::new(Object::Null))
            }
        }
    }
}
