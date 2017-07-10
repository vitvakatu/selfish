use nom::{IResult, digit, double};
use std::str;
use std::str::FromStr;
use std::collections::HashMap;

use {LispType, LispResult, LispValue};

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
    map!(many1!(none_of!("'`~@[](){}\"\t\n\r ")),
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
        many0!(ws!(expression)),
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
        many0!(ws!(expression)),
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
        many0!(ws!(pair!(keyword, expression))),
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
    map!(pair!(
        many0!(alt_complete!(
            tag!("'") => { |_| LispType::Symbol("quote".into()) } |
            tag!("@") => { |_| LispType::Symbol("deref".into()) } |
            tag!("`") => { |_| LispType::Symbol("quasiquote".into()) } |
            tag!("~@") => { |_| LispType::Symbol("splice-unquote".into()) } |
            tag!("~") => { |_| LispType::Symbol("unquote".into()) }
        )),
        alt!(atom | list | vector | hash_map)
    ),
    |(mut mac, expr)| {
        if mac.len() != 0 {
            let mut result = expr;
            mac.reverse();
            for e in mac {
                result = LispType::List(vec![LispValue::new(e), LispValue::new(result)]);
            }
            result
        } else {
            expr
        }
    })
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