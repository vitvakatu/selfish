#[macro_use]
extern crate nom;

use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

mod core;
mod reader;
mod eval;

pub use reader::Reader;
pub use core::read_eval;

pub use core::standart_environment;

#[derive(Clone, Debug, PartialEq)]
pub struct Value(Rc<Type>);

pub type List = Vec<Value>;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Atom(RefCell<Value>),
    Int(isize),
    Symbol(String),
    Str(String),
    Keyword(String),
    Boolean(bool),
    Float(f64),
    List(List),
    Vector(List),
    Map(HashMap<String, Value>),
    Func(fn(List) -> LispResult),
    Closure(Closure),
    Nothing,
}

impl std::ops::Deref for Value {
    type Target = Rc<Type>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

macro_rules! value_constructor {
    ($name:ident ($t:ident) = $repr:expr) => (
        pub fn $name(v: $t) -> Self {
            Value(Rc::new($repr(v)))
        }
    )
}

type Function = fn(List) -> LispResult;

impl Value {

    pub fn new(v: Type) -> Self {
        Value(Rc::new(v))
    }

    value_constructor!(int (isize) = Type::Int);
    value_constructor!(float (f64) = Type::Float);
    value_constructor!(boolean (bool) = Type::Boolean);
    value_constructor!(symbol (String) = Type::Symbol);
    value_constructor!(string (String) = Type::Str);
    value_constructor!(keyword (String) = Type::Keyword);
    value_constructor!(list (List) = Type::List);
    value_constructor!(vector (List) = Type::Vector);
    value_constructor!(func (Function) = Type::Func);

    pub fn atom(v: Value) -> Self {
        Value(Rc::new(Type::Atom(RefCell::new(v))))
    }

    pub fn map(v: HashMap<String, Value>) -> Self {
        Value(Rc::new(Type::Map(v)))
    }

    pub fn nothing() -> Self {
        Value(Rc::new(Type::Nothing))
    }

    pub fn closure(binds: Vec<String>, body: Value, env: Environment) -> Self {
        Value(Rc::new(
            Type::Closure(
                Closure {
                    binds,
                    body,
                    env,
                    is_macro: false,
                }
            )
        ))
    }

    pub fn macros(closure: Closure) -> Self {
        Value(Rc::new(
            Type::Closure(
                Closure {
                    is_macro: true,
                    .. closure
                }
            )
        ))
    }
}

pub type LispResult = Result<Value, Error>;

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    InvalidArg(&'static str, &'static str),
    InvalidArity(&'static str, &'static str),
    ParseError(String),
    Incomplete,
    Custom(String),
    Value(Value),
}

#[derive(Clone)]
pub struct Closure {
    pub binds: Vec<String>,
    pub body: Value,
    pub env: Environment,
    pub is_macro: bool,
}

impl PartialEq for Closure {
    fn eq(&self, _: &Closure) -> bool {
        false
    }
}

impl std::fmt::Debug for Closure {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Closure")
    }
}

fn print_str(ast: Value, pretty: bool) -> String {
    match **ast {
        Type::Boolean(ref v) => v.to_string(),
        Type::Float(ref v) => v.to_string(),
        Type::Int(ref v) => v.to_string(),
        Type::List(ref v) => {
            let mut result = String::new();
            for (i, e) in v.iter().enumerate() {
                result.push_str(&print_str(e.clone(), pretty));
                if i != v.len() - 1 {
                    result.push_str(" ");
                }
            }
            format!("({})", &result)
        },
        Type::Vector(ref v) => {
            let mut result = String::new();
            for (i, e) in v.iter().enumerate() {
                result.push_str(&print_str(e.clone(), pretty));
                if i != v.len() - 1 {
                    result.push_str(" ");
                }
            }
            format!("[{}]", &result)
        },
        Type::Map(ref v) => {
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
        Type::Str(ref v) => if pretty {
            format!("{}", v)
        } else {
            format!("\"{}\"", v)
        },
        Type::Symbol(ref v) => v.clone(),
        Type::Keyword(ref v) => format!(":{}", v),
        Type::Func(_) => "#<function>".to_owned(),
        Type::Closure(_) => "#<closure>".to_owned(),
        Type::Nothing => "".to_owned(),
        Type::Atom(ref v) => format!("atom<{}>", &print_str(v.borrow().clone(), pretty)),
    }
}

pub struct Writer {
}

impl Writer {
    pub fn print(ast: Value, pretty: bool) -> String {
        print_str(ast, pretty)
    }
}

pub type Environment = Rc<RefCell<EnvironmentStruct>>;

#[derive(Clone, Debug, PartialEq)]
pub struct EnvironmentStruct {
    data: HashMap<String, Value>,
    outer: Option<Environment>,
}

impl EnvironmentStruct {
    pub fn new(outer: Option<Environment>) -> Environment {
        Rc::new(RefCell::new(EnvironmentStruct {
            data: HashMap::new(),
            outer: outer,
        }))
    }

    pub fn top(env: &Environment) -> Environment {
        match env.borrow().outer {
            Some(ref e) => EnvironmentStruct::top(e),
            None => env.clone(),
        }
    }

    pub fn with_bindings(outer: Option<Environment>,
                         binds: Vec<String>,
                         exprs: Vec<Value>) -> Environment {
        let mut result = EnvironmentStruct {
            data: HashMap::new(),
            outer: outer,
        };
        for (b, e) in binds.into_iter().zip(exprs.into_iter()) {
            result.set(b, e);
        }
        Rc::new(RefCell::new(result))
    }

    pub fn set(&mut self, key: String, val: Value) {
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
                    Err(Error::Custom(format!("No value with name '{}' in the current scope", &key)))
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
        assert_eq!(atom(&b"true"[..]), IResult::Done(&b""[..], Type::Boolean(true)));
        assert_eq!(atom(&b"false"[..]), IResult::Done(&b""[..], Type::Boolean(false)));
        assert_eq!(atom(&b"function"[..]), IResult::Done(&b""[..], Type::Symbol("function".to_owned())));
        assert_eq!(atom(&b":keyword"[..]), IResult::Done(&b""[..], Type::Keyword("keyword".to_owned())));
    }

    #[test]
    fn numbers() {
        assert_eq!(atom(&b"32"[..]), IResult::Done(&b""[..], Type::Int(32)));
        assert_eq!(atom(&b"-32321"[..]), IResult::Done(&b""[..], Type::Int(-32321)));
    }

    #[test]
    fn floats() {
        assert_eq!(atom(&b"1.3"[..]), IResult::Done(&b""[..], Type::Float(1.3)));
    }

    #[test]
    fn strings() {
        assert_eq!(atom(&b"\"some string\""[..]), IResult::Done(&b""[..],
        Type::Str("some string".to_owned())));
        assert_eq!(atom(&b"\"some \\\"escaped\\\" string\""[..]), IResult::Done(&b""[..],
        Type::Str("some \"escaped\" string".to_owned())));
    }

    #[test]
    fn lists() {
        let list1 = Type::List(vec![
            Rc::new(Type::Symbol("somesymbol".to_owned())),
            Rc::new(Type::Int(11)),
            Rc::new(Type::Boolean(true)),
            Rc::new(Type::Float(1.3)),
            Rc::new(Type::Int(10231)),
            Rc::new(Type::Int(-30)),
            Rc::new(Type::Str("some string".to_owned())),
        ]);
        assert_eq!(list(&b"(somesymbol 11 true 1.3 10231 -30 \"some string\")"[..]),
        IResult::Done(&b""[..], list1.clone()));
        let list2 = Type::List(vec![
            Rc::new(list1),
            Rc::new(Type::Symbol("somesymbol".to_owned())),
        ]);
        assert_eq!(list(&b"((somesymbol 11 true 1.3 10231 -30 \"some string\") somesymbol)"[..]),
        IResult::Done(&b""[..], list2));
    }

    #[test]
    fn vectors() {
        let vector1 = Type::Vector(vec![
            Rc::new(Type::Symbol("somesymbol".to_owned())),
            Rc::new(Type::Int(11)),
            Rc::new(Type::Boolean(true)),
            Rc::new(Type::Float(1.3)),
            Rc::new(Type::Int(10231)),
            Rc::new(Type::Int(-30)),
            Rc::new(Type::Str("some string".to_owned())),
        ]);
        assert_eq!(vector(&b"[somesymbol 11 true 1.3 10231 -30 \"some string\"]"[..]),
        IResult::Done(&b""[..], vector1.clone()));
        let vector2 = Type::Vector(vec![
            Rc::new(vector1),
            Rc::new(Type::Symbol("somesymbol".to_owned())),
        ]);
        assert_eq!(vector(&b"[[somesymbol 11 true 1.3 10231 -30 \"some string\"] somesymbol]"[..]),
        IResult::Done(&b""[..], vector2));
    }
}
