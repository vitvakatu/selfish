use nom::{IResult, digit, double, not_line_ending, line_ending};
use std::str;
use std::str::FromStr;
use std::collections::HashMap;

use {Type, LispResult, Value};

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

named!(number<Type>,
    alt_complete!(double => { |v| Type::Float(v as f64) } |
                  integer_number => { |v| Type::Int(v as isize) }
                  )
);

named!(symbol<String>,
    map!(many1!(none_of!("'`~@[](){};\"\t\n\r ")),
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

named!(atom<&[u8], Type>,
    alt_complete!(number |
                  keyword => { |v| Type::Keyword(v) } |
                  string => { |v| Type::Str(v) } |
                  symbol => { |v: String| match v.as_ref() {
                      "true" => Type::Boolean(true),
                      "false" => Type::Boolean(false),
                      _ => Type::Symbol(v),
                      }
                  }
));

named!(list<&[u8], Type>,
    map!(delimited!(
        char!('('),
        many0!(ws!(expression)),
        char!(')')
    ),
    |v| {
        let mut result = Vec::new();
        for e in v.into_iter().filter(|e| e != &Type::Nothing) {
            result.push(Value::new(e));
        }
        Type::List(result)
    })
);

named!(vector<&[u8], Type>,
    map!(delimited!(
        char!('['),
        many0!(ws!(expression)),
        char!(']')
    ),
    |v| {
        let mut result = Vec::new();
        for e in v.into_iter().filter(|e| e != &Type::Nothing) {
            result.push(Value::new(e));
        }
        Type::Vector(result)
    })
);

named!(hash_map<Type>,
    map!(delimited!(
        char!('{'),
        many0!(ws!(pair!(keyword, expression))),
        char!('}')
    ),
    |v| {
        let mut result = HashMap::new();
        for e in v.into_iter().filter(|e| e.1 != Type::Nothing) {
            result.insert(e.0, Value::new(e.1));
        }
        Type::Map(result)
    })
);

named!(expression<&[u8], Type>,
    map!(pair!(
        many0!(alt_complete!(
            tag!("'") => { |_| Type::Symbol("quote".into()) } |
            tag!("@") => { |_| Type::Symbol("deref".into()) } |
            tag!("`") => { |_| Type::Symbol("quasiquote".into()) } |
            tag!("~@") => { |_| Type::Symbol("splice-unquote".into()) } |
            tag!("~") => { |_| Type::Symbol("unquote".into()) }
        )),
        alt!(atom | list | vector | hash_map | comment)
    ),
    |(mut mac, expr)| {
        if !mac.is_empty() {
            let mut result = expr;
            mac.reverse();
            for e in mac {
                result = Type::List(vec![Value::new(e), Value::new(result)]);
            }
            result
        } else {
            expr
        }
    })
);

named!(comment<&[u8], Type>,
    do_parse!(
        tag!(";") >>
        not_line_ending >>
        line_ending >>
        (Type::Nothing)
    )
);

named!(program<&[u8], Value>,
    map!(many1!(ws!(expression)),
    |exprs| {
        if exprs.len() > 1 {
            let mut values: Vec<Value> = exprs
                                                .into_iter()
                                                .filter(|e| e != &Type::Nothing)
                                                .map(Value::new)
                                                .collect();
            values.insert(0, Value::symbol("do".into()));
            Value::list(values)
        } else {
            Value::new(exprs[0].clone())
        }
    })
);

pub struct Reader {
}

impl Reader {
    pub fn read(slice: &[u8]) -> LispResult {
        use Error;
        match program(slice) {
            IResult::Done(_, result) => Ok(result),
            IResult::Error(e) => Err(Error::ParseError(format!("{}", e))),
            IResult::Incomplete(_) => Err(Error::Incomplete),
        }
    }
}