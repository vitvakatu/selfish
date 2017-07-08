use std::rc::Rc;

#[macro_use]
extern crate nom;

use nom::{IResult, alphanumeric, alpha, digit, double};

use std::str;
use std::str::FromStr;

pub type LispValue = Rc<LispType>;
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
}

named!(string<&[u8], String>,
    delimited!(
        tag!("\""),
        fold_many0!(//is_not!("\""),
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
    map!(do_parse!(
        first: map_res!(alpha, str::from_utf8) >>
        rest: opt!(complete!(map_res!(alphanumeric, str::from_utf8))) >>
        (first, rest)
    ),
    |(first, rest)| {
        let mut string = first.to_owned();
        string.push_str(rest.unwrap_or(""));
        string
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
                  symbol => { |v| LispType::Symbol(v) } |
                  keyword => { |v| LispType::Keyword(v) } |
                  string => { |v| LispType::Str(v) })
);

named!(list<&[u8], LispType>,
    map!(delimited!(
        char!('('),
        many0!(ws!(alt!(atom | list | vector))),
        char!(')')
    ),
    |v| {
        let mut result = Vec::new();
        for e in v {
            result.push(Rc::new(e));
        }
        LispType::List(result)
    })
);

named!(vector<&[u8], LispType>,
    map!(delimited!(
        char!('['),
        many0!(ws!(alt!(atom | list | vector))),
        char!(']')
    ),
    |v| {
        let mut result = Vec::new();
        for e in v {
            result.push(Rc::new(e));
        }
        LispType::Vector(result)
    })
);

named!(expression<&[u8], LispType>,
    alt!(atom | list)
);

pub struct Reader {
}

impl Reader {
    pub fn read(slice: &[u8]) -> Result<LispValue, &str> {
        match expression(slice) {
            IResult::Done(_, result) => Ok(Rc::new(result)),
            _ => Err("Something gone wrong..."),
        }
    }
}

fn print_str(ast: LispValue) -> String {
    match *ast {
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
        LispType::Str(ref v) => format!("\"{}\"", v),
        LispType::Symbol(ref v) => v.clone(),
        LispType::Keyword(ref v) => format!(":{}", v)
    }
}

pub struct Writer {
}

impl Writer {
    pub fn print(ast: LispValue) -> String {
        print_str(ast)
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
