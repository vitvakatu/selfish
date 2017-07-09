use std::rc::Rc;

#[macro_use]
extern crate nom;

use nom::{IResult, digit, double};

use std::str;
use std::str::FromStr;
use std::collections::HashMap;
use std::cell::RefCell;

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

/*impl LispClosure {
    pub fn new(binds: LispList, exprs: LispList, outer_env: Environment) -> Self {
        LispClosure {
            binds,
            exprs,
            outer_env,
        }
    }
}*/

/*impl LispType {
    pub fn apply(&self, args: LispList) -> LispResult {
        match *self {
            LispType::Func(f) => {
                (f)(args)
            },
            LispType::Closure(ref c) => {
                let new_env = EnvironmentStruct::with_bindings(
                    c.outer_env.clone(),
                    c.binds,
                    args
                );

            }
        }
    }
}*/

named!(string<&[u8], String>,
    delimited!(
        tag!("\""),
        fold_many0!(
            alt!(
                is_not!("\\\"") |
                map!(
                    complete!(tag!("\\\"")),
                    |_| &b"\""[..]
                )
            ),
            String::new(),
            |mut acc: String, bytes: &[u8]| {
                acc.push_str(str::from_utf8(bytes).expect("Wrong utf8"));
                acc
            }
        ),
        tag!("\"")
    )
);

named!(integer_number<&[u8], isize>,
    map!(do_parse!(
        sign: opt!(alt!(char!('-') | char!('+'))) >>
        number: map_res!(map_res!(digit, str::from_utf8), isize::from_str) >>
        (number, sign)
    ),
    | (number, sign) | {
        match sign {
            Some(_) => -number,
            None => number,
        }
    })
);

named!(number<LispType>,
    alt_complete!(double => { |v| LispType::Float(v as f64) } |
                  integer_number => { |v| LispType::Int(v as isize) }
                  )
);

named!(symbol<String>,
    map!(many1!(none_of!("[](){}\"\t\n\r ")),
    |v| {
        v.into_iter().collect()
    })
);

named!(keyword<String>,
    preceded!(
        tag!(":"),
        symbol
    )
);

named!(atom<&[u8], LispType>,
    alt_complete!(number |
                  tag!("true") => { |_| LispType::Boolean(true) } |
                  tag!("false") => { |_| LispType::Boolean(false) } |
                  keyword => { |v| LispType::Keyword(v) } |
                  string => { |v| LispType::Str(v) } |
                  symbol => { |v| LispType::Symbol(v) }
));

named!(list<&[u8], LispType>,
    map!(delimited!(
        char!('('),
        many0!(ws!(alt!(atom | list | vector | hash_map))),
        char!(')')
    ),
    |v| {
        let mut result = Vec::new();
        for e in v {
            result.push(LispValue::new(e));
        }
        LispType::List(result)
    })
);

named!(vector<&[u8], LispType>,
    map!(delimited!(
        char!('['),
        many0!(ws!(alt!(atom | list | vector | hash_map))),
        char!(']')
    ),
    |v| {
        let mut result = Vec::new();
        for e in v {
            result.push(LispValue::new(e));
        }
        LispType::Vector(result)
    })
);

named!(hash_map<LispType>,
    map!(delimited!(
        char!('{'),
        many0!(ws!(pair!(keyword, alt!(atom | list | vector | hash_map)))),
        char!('}')
    ),
    |v| {
        let mut result = HashMap::new();
        for e in v {
            result.insert(e.0, LispValue::new(e.1));
        }
        LispType::Map(result)
    })
);

named!(expression<&[u8], LispType>,
    alt!(atom | list | vector | hash_map)
);

pub struct Reader {
}

impl Reader {
    pub fn read(slice: &[u8]) -> LispResult {
        match expression(slice) {
            IResult::Done(_, result) => Ok(LispValue::new(result)),
            _ => Err("Something gone wrong...".to_owned()),
        }
    }
}

fn print_str(ast: LispValue) -> String {
    match **ast {
        LispType::Boolean(ref v) => v.to_string(),
        LispType::Float(ref v) => v.to_string(),
        LispType::Int(ref v) => v.to_string(),
        LispType::List(ref v) => {
            let mut result = String::new();
            for (i, e) in v.iter().enumerate() {
                result.push_str(&print_str(e.clone()));
                if i != v.len() - 1 {
                    result.push_str(" ");
                }
            }
            format!("({})", &result)
        },
        LispType::Vector(ref v) => {
            let mut result = String::new();
            for (i, e) in v.iter().enumerate() {
                result.push_str(&print_str(e.clone()));
                if i != v.len() - 1 {
                    result.push_str(" ");
                }
            }
            format!("[{}]", &result)
        },
        LispType::Map(ref v) => {
            let mut result = String::new();
            for (i, (key, value)) in v.iter().enumerate() {
                result.push_str(&format!(":{} {}", &key, &print_str(value.clone())));
                if i != v.len() - 1 {
                    result.push_str(" ");
                }
            }
            format!("{{{}}}", &result)
        },
        LispType::Str(ref v) => format!("\"{}\"", v),
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
    pub fn print(ast: LispValue) -> String {
        print_str(ast)
    }
}
macro_rules! arithmetic_function {
    ($name: ident, $op: expr, $finit:expr) => (
        fn $name(input: LispList) -> LispResult {
            let err = Err("Invalid types: arithmetic operations works \
                        with ints and floats only".to_owned());
            if input.len() == 0 {
                return Ok(LispValue::int(0));
            }
            if input.len() == 1 {
                return Ok(input[0].clone());
            }
            let mut result = match **input[0] {
                LispType::Int(v) => LispType::Int(v),
                LispType::Float(v) => LispType::Float(v),
                _ => return err,
            };
            for e in &input[1..] {
                match ***e {
                    LispType::Int(i) => {
                        result = match result {
                            LispType::Int(v) => LispType::Int($op(v, i)),
                            LispType::Float(v) => LispType::Float($op(v, i as f64)),
                            _ => unreachable!(),
                        };
                    },
                    LispType::Float(f) => {
                        result = match result {
                            LispType::Int(v) => LispType::Float($op(v as f64, f)),
                            LispType::Float(v) => LispType::Float($op(v, f)),
                            _ => unreachable!(),
                        };
                    },
                    _ => return err,
                }
            }
            Ok(LispValue::new(result.clone()))
        }
    )
}

arithmetic_function!(add, std::ops::Add::add, 0.0);
arithmetic_function!(sub, std::ops::Sub::sub, 0.0);
arithmetic_function!(mult, std::ops::Mul::mul, 1.0);
arithmetic_function!(div, std::ops::Div::div, 1.0);

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

fn internal_print(args: LispList) -> LispResult {
    if args.len() != 1 {
        return Err("Invalid arity of 'print' function".to_owned());
    }
    println!("{}", &Writer::print(args[0].clone()));
    Ok(LispValue::nothing())
}

fn internal_list(args: LispList) -> LispResult {
    let mut result = Vec::new();
    for e in args {
        result.push(e.clone());
    }
    Ok(LispValue::list(result))
}

fn internal_listq(args: LispList) -> LispResult {
    if args.len() != 1 {
        return Err("Invalid arity of 'list?' function".to_owned());
    }
    if let LispType::List(_) = **args[0] {
        Ok(LispValue::boolean(true))
    } else {
        Ok(LispValue::boolean(false))
    }
}

fn internal_emptyq(args: LispList) -> LispResult {
    if args.len() != 1 {
        return Err("Invalid arity of 'empty?' function".to_owned());
    }
    match **args[0] {
        LispType::List(ref v) | LispType::Vector(ref v) => {
            if v.len() == 0 {
                Ok(LispValue::boolean(true))
            } else {
                Ok(LispValue::boolean(false))
            }
        },
        _ => Err("Invalid argument of 'empty?' function (must be the list or vector)".to_owned()),
    }
}

fn internal_count(args: LispList) -> LispResult {
    if args.len() != 1 {
        return Err("Invalid arity of 'count' function".to_owned());
    }
    match **args[0] {
        LispType::List(ref v) | LispType::Vector(ref v) => {
            Ok(LispValue::int(v.len() as isize))
        },
        _ => Err("Invalid argument of 'count' function (must be the list or vector)".to_owned()),
    }
}

fn internal_eq(args: LispList) -> LispResult {
    if args.len() != 2 {
        return Err("Invalid arity of '=' function".to_owned());
    }
    if **args[0] == **args[1] {
        Ok(LispValue::boolean(true))
    } else {
        Ok(LispValue::boolean(false))
    }
}

macro_rules! construct_cmp {
    ($args:ident, $func:ident, $func_name:expr) => (
        match **$args[0] {
            LispType::Int(fst) => {
                match **$args[1] {
                    LispType::Float(snd) => if (fst as f64).$func(&snd) {
                        Ok(LispValue::boolean(true))
                    } else {
                        Ok(LispValue::boolean(false))
                    },
                    LispType::Int(snd) => if fst.$func(&snd) {
                        Ok(LispValue::boolean(true))
                    } else {
                        Ok(LispValue::boolean(false))
                    },
                    _ => Err(format!("Invalid arguments of '{}' function", $func_name))
                }
            },
            LispType::Float(fst) => {
                match **$args[1] {
                    LispType::Float(snd) => if fst.$func(&snd) {
                        Ok(LispValue::boolean(true))
                    } else {
                        Ok(LispValue::boolean(false))
                    },
                    LispType::Int(snd) => if fst.$func(&(snd as f64)) {
                        Ok(LispValue::boolean(true))
                    } else {
                        Ok(LispValue::boolean(false))
                    },
                    _ => Err(format!("Invalid arguments of '{}' function", $func_name))
                }
            },
            _ => Err(format!("Invalid arguments of '{}' function", $func_name))
        }
    )
}

fn internal_lt(args: LispList) -> LispResult {
    if args.len() != 2 {
        return Err("Invalid arity of '<' function".to_owned());
    }
    construct_cmp!(args, lt, "<")
}

fn internal_le(args: LispList) -> LispResult {
    if args.len() != 2 {
        return Err("Invalid arity of '<=' function".to_owned());
    }
    construct_cmp!(args, le, "<=")
}

fn internal_gt(args: LispList) -> LispResult {
    if args.len() != 2 {
        return Err("Invalid arity of '>' function".to_owned());
    }
    construct_cmp!(args, gt, ">")
}

fn internal_ge(args: LispList) -> LispResult {
    if args.len() != 2 {
        return Err("Invalid arity of '>=' function".to_owned());
    }
    construct_cmp!(args, ge, ">=")
}

pub fn standart_environment() -> Environment {
    let result = EnvironmentStruct::new(None);
    {
        let mut r = result.borrow_mut();
        r.set("+".to_owned(), LispValue::func(add));
        r.set("-".to_owned(), LispValue::func(sub));
        r.set("*".to_owned(), LispValue::func(mult));
        r.set("/".to_owned(), LispValue::func(div));

        r.set("print".to_owned(), LispValue::func(internal_print));
        r.set("list".to_owned(), LispValue::func(internal_list));
        r.set("list?".to_owned(), LispValue::func(internal_listq));
        r.set("empty?".to_owned(), LispValue::func(internal_emptyq));
        r.set("count".to_owned(), LispValue::func(internal_count));
        r.set("=".to_owned(), LispValue::func(internal_eq));
        r.set("<=".to_owned(), LispValue::func(internal_le));
        r.set("<".to_owned(), LispValue::func(internal_lt));
        r.set(">=".to_owned(), LispValue::func(internal_ge));
        r.set(">".to_owned(), LispValue::func(internal_gt));
    }
    result
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
