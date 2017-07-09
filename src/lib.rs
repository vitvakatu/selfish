#[macro_use]
extern crate nom;

use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

mod core;
mod reader;
mod eval;

pub use reader::Reader;
pub use eval::eval;

pub use core::standart_environment;

#[derive(Clone, Debug, PartialEq)]
pub struct LispValue(Rc<LispType>);

pub type LispList = Vec<LispValue>;

#[derive(Debug, PartialEq, Clone)]
pub enum LispType {
    Int(isize),
    Symbol(String),
    Str(String),
    Keyword(String),
    Boolean(bool),
    Float(f64),
    List(LispList),
    Vector(LispList),
    Map(HashMap<String, LispValue>),
    Func(fn(LispList) -> LispResult),
    Closure(LispClosure),
    Nothing,
}

impl std::ops::Deref for LispValue {
    type Target = Rc<LispType>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

macro_rules! value_constructor {
    ($name:ident ($t:ident) = $repr:expr) => (
        pub fn $name(v: $t) -> Self {
            LispValue(Rc::new($repr(v)))
        }
    )
}

type LispFunction = fn(LispList) -> LispResult;

impl LispValue {

    pub fn new(v: LispType) -> Self {
        LispValue(Rc::new(v))
    }

    value_constructor!(int (isize) = LispType::Int);
    value_constructor!(float (f64) = LispType::Float);
    value_constructor!(boolean (bool) = LispType::Boolean);
    value_constructor!(symbol (String) = LispType::Symbol);
    value_constructor!(string (String) = LispType::Str);
    value_constructor!(keyword (String) = LispType::Keyword);
    value_constructor!(list (LispList) = LispType::List);
    value_constructor!(vector (LispList) = LispType::Vector);
    value_constructor!(func (LispFunction) = LispType::Func);

    pub fn map(v: HashMap<String, LispValue>) -> Self {
        LispValue(Rc::new(LispType::Map(v)))
    }

    pub fn nothing() -> Self {
        LispValue(Rc::new(LispType::Nothing))
    }

    pub fn closure(binds: Vec<String>, body: LispValue, env: Environment) -> Self {
        LispValue(Rc::new(
            LispType::Closure(
                LispClosure {
                    binds,
                    body,
                    env,
                }
            )
        ))
    }
}

pub type LispResult = Result<LispValue, String>;

#[derive(Clone)]
pub struct LispClosure {
    pub binds: Vec<String>,
    pub body: LispValue,
    pub env: Environment,
}

impl PartialEq for LispClosure {
    fn eq(&self, _: &LispClosure) -> bool {
        false
    }
}

impl std::fmt::Debug for LispClosure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "LispClosure")
    }
}

fn print_str(ast: LispValue, pretty: bool) -> String {
    match **ast {
        LispType::Boolean(ref v) => v.to_string(),
        LispType::Float(ref v) => v.to_string(),
        LispType::Int(ref v) => v.to_string(),
        LispType::List(ref v) => {
            let mut result = String::new();
            for (i, e) in v.iter().enumerate() {
                result.push_str(&print_str(e.clone(), pretty));
                if i != v.len() - 1 {
                    result.push_str(" ");
                }
            }
            format!("({})", &result)
        },
        LispType::Vector(ref v) => {
            let mut result = String::new();
            for (i, e) in v.iter().enumerate() {
                result.push_str(&print_str(e.clone(), pretty));
                if i != v.len() - 1 {
                    result.push_str(" ");
                }
            }
            format!("[{}]", &result)
        },
        LispType::Map(ref v) => {
            let mut result = String::new();
            for (i, (key, value)) in v.iter().enumerate() {
                result.push_str(&format!(":{} {}", &key,
                                &print_str(value.clone(), pretty)));
                if i != v.len() - 1 {
                    result.push_str(" ");
                }
            }
            format!("{{{}}}", &result)
        },
        LispType::Str(ref v) => if pretty {
            format!("{}", v)
        } else {
            format!("\"{}\"", v)
        },
        LispType::Symbol(ref v) => v.clone(),
        LispType::Keyword(ref v) => format!(":{}", v),
        LispType::Func(_) => "#<function>".to_owned(),
        LispType::Closure(_) => "#<closure>".to_owned(),
        LispType::Nothing => "".to_owned(),
    }
}

pub struct Writer {
}

impl Writer {
    pub fn print(ast: LispValue, pretty: bool) -> String {
        print_str(ast, pretty)
    }
}

pub type Environment = Rc<RefCell<EnvironmentStruct>>;

#[derive(Clone, Debug, PartialEq)]
pub struct EnvironmentStruct {
    data: HashMap<String, LispValue>,
    outer: Option<Environment>,
}

impl EnvironmentStruct {
    pub fn new(outer: Option<Environment>) -> Environment {
        Rc::new(RefCell::new(EnvironmentStruct {
            data: HashMap::new(),
            outer: outer,
        }))
    }

    pub fn with_bindings(outer: Option<Environment>,
                         binds: Vec<String>,
                         exprs: Vec<LispValue>) -> Environment {
        let mut result = EnvironmentStruct {
            data: HashMap::new(),
            outer: outer,
        };
        for (b, e) in binds.into_iter().zip(exprs.into_iter()) {
            result.set(b, e);
        }
        Rc::new(RefCell::new(result))
    }

    pub fn set(&mut self, key: String, val: LispValue) {
        self.data.insert(key, val);
    }

    pub fn find(&self, key: String) -> Option<Environment> {
        if self.data.contains_key(&key) {
            Some(Rc::new(RefCell::new(self.clone())))
        } else {
            match self.outer {
                Some(ref outer) => {
                    let outer = outer.borrow();
                    outer.find(key)
                },
                None => None,
            }
        }
    }

    pub fn get(&self, key: String) -> LispResult {
        match self.data.get(&key) {
            Some(v) => Ok(v.clone()),
            None => {
                if let Some(env) = self.find(key.clone()) {
                    env.borrow().get(key)
                } else {
                    Err(format!("No value with name '{}' in the current scope", &key))
                }
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use ::*;
    #[test]
    fn atoms() {
        assert_eq!(atom(&b"true"[..]), IResult::Done(&b""[..], LispType::Boolean(true)));
        assert_eq!(atom(&b"false"[..]), IResult::Done(&b""[..], LispType::Boolean(false)));
        assert_eq!(atom(&b"function"[..]), IResult::Done(&b""[..], LispType::Symbol("function".to_owned())));
        assert_eq!(atom(&b":keyword"[..]), IResult::Done(&b""[..], LispType::Keyword("keyword".to_owned())));
    }

    #[test]
    fn numbers() {
        assert_eq!(atom(&b"32"[..]), IResult::Done(&b""[..], LispType::Int(32)));
        assert_eq!(atom(&b"-32321"[..]), IResult::Done(&b""[..], LispType::Int(-32321)));
    }

    #[test]
    fn floats() {
        assert_eq!(atom(&b"1.3"[..]), IResult::Done(&b""[..], LispType::Float(1.3)));
    }

    #[test]
    fn strings() {
        assert_eq!(atom(&b"\"some string\""[..]), IResult::Done(&b""[..],
        LispType::Str("some string".to_owned())));
        assert_eq!(atom(&b"\"some \\\"escaped\\\" string\""[..]), IResult::Done(&b""[..],
        LispType::Str("some \"escaped\" string".to_owned())));
    }

    #[test]
    fn lists() {
        let list1 = LispType::List(vec![
            Rc::new(LispType::Symbol("somesymbol".to_owned())),
            Rc::new(LispType::Int(11)),
            Rc::new(LispType::Boolean(true)),
            Rc::new(LispType::Float(1.3)),
            Rc::new(LispType::Int(10231)),
            Rc::new(LispType::Int(-30)),
            Rc::new(LispType::Str("some string".to_owned())),
        ]);
        assert_eq!(list(&b"(somesymbol 11 true 1.3 10231 -30 \"some string\")"[..]),
        IResult::Done(&b""[..], list1.clone()));
        let list2 = LispType::List(vec![
            Rc::new(list1),
            Rc::new(LispType::Symbol("somesymbol".to_owned())),
        ]);
        assert_eq!(list(&b"((somesymbol 11 true 1.3 10231 -30 \"some string\") somesymbol)"[..]),
        IResult::Done(&b""[..], list2));
    }

    #[test]
    fn vectors() {
        let vector1 = LispType::Vector(vec![
            Rc::new(LispType::Symbol("somesymbol".to_owned())),
            Rc::new(LispType::Int(11)),
            Rc::new(LispType::Boolean(true)),
            Rc::new(LispType::Float(1.3)),
            Rc::new(LispType::Int(10231)),
            Rc::new(LispType::Int(-30)),
            Rc::new(LispType::Str("some string".to_owned())),
        ]);
        assert_eq!(vector(&b"[somesymbol 11 true 1.3 10231 -30 \"some string\"]"[..]),
        IResult::Done(&b""[..], vector1.clone()));
        let vector2 = LispType::Vector(vec![
            Rc::new(vector1),
            Rc::new(LispType::Symbol("somesymbol".to_owned())),
        ]);
        assert_eq!(vector(&b"[[somesymbol 11 true 1.3 10231 -30 \"some string\"] somesymbol]"[..]),
        IResult::Done(&b""[..], vector2));
    }
}
