use {List, LispResult, Error, Value, Type, Writer, Environment, EnvironmentStruct};

// atoms
fn atom(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("atom", "1"));
    }
    let t = args[0].clone();
    Ok(Value::atom(t))
}

fn deref(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("deref", "1"));
    }
    if let Type::Atom(ref v) = **args[0] {
        Ok(v.borrow().clone())
    } else {
        Err(Error::InvalidArg("deref", "atom"))
    }
}

fn reset(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity("reset!", "2"));
    }
    if let Type::Atom(ref v) = **args[0] {
        *v.borrow_mut() = args[1].clone();
        Ok(v.borrow().clone())
    } else {
        Err(Error::InvalidArg("reset!", "atom and any other value"))
    }
}

fn swap(args: List) -> LispResult {
    if args.len() < 2 {
        return Err(Error::InvalidArity("swap!", ">= 2"));
    }
    let mut func_args = args[2..].to_vec();
    if let Type::Atom(ref v) = **args[0] {
        let val = v.borrow().clone();
        func_args.insert(0, val);
        match **args[1].clone() {
            Type::Closure(ref closure) => {
                let new_env = EnvironmentStruct::with_bindings(
                    Some(closure.env.clone()),
                    closure.binds.clone(),
                    func_args,
                ).map_err(|e| Error::BindError(e))?;
                use eval::eval;
                let new_val = eval(closure.body.clone(), new_env.clone())?;
                *v.borrow_mut() = new_val.clone();
                Ok(new_val)
            }
            Type::Func(func) => {
                let new_val = func(func_args)?;
                *v.borrow_mut() = new_val.clone();
                Ok(new_val)
            }
            _ => Err(Error::InvalidArg(
                "swap!",
                "atom, either closure or function, any values",
            )),
        }
    } else {
        Err(Error::InvalidArg(
            "swap!",
            "atom, either closure or function, any values",
        ))
    }
}

// constructors
fn keyword(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("keyword", "1"));
    }
    if let Type::Str(ref s) = **args[0] {
        Ok(Value::keyword(s.clone()))
    } else {
        Err(Error::InvalidArg("keyword", "string"))
    }
}

fn str(args: List) -> LispResult {
    let mut result = String::new();
    for e in args {
        result.push_str(&Writer::print(e.clone(), true));
    }
    Ok(Value::string(result))
}

fn symbol(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("symbol", "1"));
    }
    if let Type::Str(ref s) = **args[0] {
        Ok(Value::symbol(s.clone()))
    } else {
        Err(Error::InvalidArg("symbol", "string"))
    }
}

fn vector(args: List) -> LispResult {
    let mut result = Vec::new();
    for e in args {
        result.push(e.clone());
    }
    Ok(Value::vector(result))
}

// hash-maps
fn assoc(args: List) -> LispResult {
    if args.len() < 3 {
        return Err(Error::InvalidArity("assoc", ">= 3"));
    }
    if let Type::Map(ref map) = **args[0] {
        let mut result = map.clone();
        if args[1..].len() % 2 != 0 {
            return Err(Error::InvalidArg(
                "assoc",
                "hash-map followed by equal \
                 amount of keywords (odd args) and values (even args)",
            ));
        }
        for e in args[1..].chunks(2) {
            match **e[0] {
                Type::Keyword(ref s) => {
                    result.insert(s.clone(), e[1].clone());
                }
                _ => {
                    return Err(Error::InvalidArg(
                        "assoc",
                        "hash-map followed \
                         by equal amount of keywords (odd args) and values (even args)",
                    ))
                }
            }
        }
        Ok(Value::map(result))
    } else {
        Err(Error::InvalidArg(
            "assoc",
            "hash-map followed by equal amount of \
             keywords (odd args) and values (even args)",
        ))
    }
}

fn containsq(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity("contains?", "2"));
    }
    if let Type::Map(ref map) = **args[0] {
        if let Type::Keyword(ref k) = **args[1] {
            Ok(Value::boolean(map.contains_key(k)))
        } else {
            Err(Error::InvalidArg(
                "contains?",
                "hash-map followed by keyword",
            ))
        }
    } else {
        Err(Error::InvalidArg(
            "contains?",
            "hash-map followed by keyword",
        ))
    }
}

fn dissoc(args: List) -> LispResult {
    if args.len() < 2 {
        return Err(Error::InvalidArity("dissoc", ">= 2"));
    }
    if let Type::Map(ref map) = **args[0] {
        let mut result = map.clone();
        for e in args[1..].chunks(2) {
            match **e[0] {
                Type::Keyword(ref s) => {
                    result.remove(s);
                }
                _ => {
                    return Err(Error::InvalidArg(
                        "dissoc",
                        "hash-map followed \
                         by any amount of keywords",
                    ))
                }
            }
        }
        Ok(Value::map(result))
    } else {
        Err(Error::InvalidArg(
            "dissoc",
            "hash-map followed by any \
             amount of keywords",
        ))
    }
}

fn get(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity("get", "2"));
    }
    if let Type::Map(ref map) = **args[0] {
        if let Type::Keyword(ref k) = **args[1] {
            match map.get(k) {
                Some(v) => Ok(v.clone()),
                None => Ok(Value::list(vec![])),
            }
        } else {
            Err(Error::InvalidArg("get", "hash-map followed by keyword"))
        }
    } else {
        Err(Error::InvalidArg("get", "hash-map followed by keyword"))
    }
}

fn hash_map(args: List) -> LispResult {
    use std::collections::HashMap;
    let mut result: HashMap<String, Value> = HashMap::new();
    if args.len() % 2 != 0 {
        return Err(Error::InvalidArg(
            "hash-map",
            "equal amount of keywords \
             (odd args) and values (even args)",
        ));
    }
    for e in args.as_slice().chunks(2) {
        match **e[0] {
            Type::Keyword(ref s) => {
                result.insert(s.clone(), e[1].clone());
            }
            _ => {
                return Err(Error::InvalidArg(
                    "hash-map",
                    "equal amount of \
                     keywords (odd args) and values (even args)",
                ))
            }
        }
    }
    Ok(Value::map(result))
}


fn keys(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("keys", "1"));
    }
    if let Type::Map(ref map) = **args[0] {
        Ok(Value::list(
            map.keys().cloned().map(Value::keyword).collect(),
        ))
    } else {
        Err(Error::InvalidArg("keys", "hash-map"))
    }
}

fn values(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("values", "1"));
    }
    if let Type::Map(ref map) = **args[0] {
        Ok(Value::list(map.values().cloned().collect()))
    } else {
        Err(Error::InvalidArg("values", "hash-map"))
    }
}

fn print(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("print", "1"));
    }
    print!("{}", &Writer::print(args[0].clone(), true));
    Ok(Value::nothing())
}

fn println(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("println", "1"));
    }
    println!("{}", &Writer::print(args[0].clone(), true));
    Ok(Value::nothing())
}

fn read_string(args: List) -> LispResult {
    use Reader;
    if args.len() != 1 {
        return Err(Error::InvalidArity("read-string", "1"));
    }
    if let Type::Str(ref s) = **args[0] {
        Reader::read(s.as_bytes())
    } else {
        Err(Error::InvalidArg("read-string", "string"))
    }
}

fn slurp(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("slurp", "1"));
    }
    use std::io::prelude::*;
    use std::fs::File;
    if let Type::Str(ref s) = **args[0] {
        let mut f = File::open(s).unwrap();
        let mut buffer = String::new();
        f.read_to_string(&mut buffer).unwrap();
        Ok(Value::string(buffer))
    } else {
        Err(Error::InvalidArg("slurp", "string"))
    }
}

fn apply(args: List) -> LispResult {
    if args.len() < 2 {
        return Err(Error::InvalidArity("apply", ">= 2"));
    }
    let last = args.len() - 1;
    match **args[last] {
        Type::List(ref v) | Type::Vector(ref v) => {
            let mut func_args = args[1..last].to_vec();
            func_args.extend(v.clone());
            match **args[0] {
                Type::Func(func) => return func(func_args),
                Type::Closure(ref closure) => {
                    let new_env = EnvironmentStruct::with_bindings(
                        Some(closure.env.clone()),
                        closure.binds.clone(),
                        func_args,
                    ).map_err(|e| Error::BindError(e))?;
                    use eval::eval;
                    let new_val = eval(closure.body.clone(), new_env.clone())?;
                    Ok(new_val)
                }
                _ => Err(Error::InvalidArg(
                    "apply",
                    "function followed by any values and list",
                )),
            }
        }
        _ => Err(Error::InvalidArg(
            "apply",
            "function followed by any values and list",
        )),
    }
}

// lists
fn concat(args: List) -> LispResult {
    let mut result = Vec::new();
    for e in args {
        match **e {
            Type::List(ref v) => result.extend(v.clone()),
            _ => return Err(Error::InvalidArg("concat", "any amount of lists")),
        }
    }
    Ok(Value::list(result))
}

fn cons(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity("cons", "2"));
    }
    if let Type::List(ref v) = **args[1].clone() {
        let mut result = v.clone();
        result.insert(0, args[0].clone());
        Ok(Value::list(result))
    } else {
        Err(Error::InvalidArg("cons", "any value and list"))
    }
}

fn count(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("count", "1"));
    }
    match **args[0] {
        Type::List(ref v) | Type::Vector(ref v) => Ok(Value::int(v.len() as isize)),
        _ => Err(Error::InvalidArg("count", "either list or vector")),
    }
}

fn first(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("first", "1"));
    }
    match **args[0] {
        Type::List(ref v) | Type::Vector(ref v) => {
            if v.len() >= 1 {
                Ok(v[0].clone())
            } else {
                Ok(Value::list(vec![]))
            }
        }
        _ => Err(Error::InvalidArg("first", "list or vector")),
    }
}

fn list(args: List) -> LispResult {
    let mut result = Vec::new();
    for e in args {
        result.push(e.clone());
    }
    Ok(Value::list(result))
}

fn nth(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity("nth", "2"));
    }
    let index = match **args[1] {
        Type::Int(i) => i,
        _ => {
            return Err(Error::InvalidArg(
                "nth",
                "list or vector followed by integer number",
            ))
        }
    };
    match **args[0] {
        Type::List(ref v) | Type::Vector(ref v) => {
            if v.len() <= index as usize || index < 0 {
                return Err(Error::Value(Value::string("index out of bounds".into())));
            }
            Ok(v[index as usize].clone())
        }
        _ => Err(Error::InvalidArg(
            "nth",
            "list or vector followed by integer number",
        )),
    }
}

fn rest(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("rest", "1"));
    }
    match **args[0] {
        Type::List(ref v) | Type::Vector(ref v) => {
            if v.len() >= 2 {
                Ok(Value::list(v[1..].to_vec()))
            } else {
                Ok(Value::list(vec![]))
            }
        }
        _ => Err(Error::InvalidArg("rest", "list or vector")),
    }
}

// math and ordering
macro_rules! arithmetic_function {
    ($name: ident, $op: expr, $finit:expr) => (
        fn $name(input: List) -> LispResult {
            let err = Err(Error::InvalidArg("arithmetic function",
                "any amount of integer or floating point numbers"));
            if input.len() == 0 {
                return Ok(Value::int(0));
            }
            if input.len() == 1 {
                return Ok(input[0].clone());
            }
            let mut result = match **input[0] {
                Type::Int(v) => Type::Int(v),
                Type::Float(v) => Type::Float(v),
                _ => return err,
            };
            for e in &input[1..] {
                match ***e {
                    Type::Int(i) => {
                        result = match result {
                            Type::Int(v) => Type::Int($op(v, i)),
                            Type::Float(v) => Type::Float($op(v, i as f64)),
                            _ => unreachable!(),
                        };
                    },
                    Type::Float(f) => {
                        result = match result {
                            Type::Int(v) => Type::Float($op(v as f64, f)),
                            Type::Float(v) => Type::Float($op(v, f)),
                            _ => unreachable!(),
                        };
                    },
                    _ => return err,
                }
            }
            Ok(Value::new(result.clone()))
        }
    )
}

use std::ops::{Add, Sub, Div, Mul};
arithmetic_function!(add, Add::add, 0.0);
arithmetic_function!(sub, Sub::sub, 0.0);
arithmetic_function!(mult, Mul::mul, 1.0);
arithmetic_function!(div, Div::div, 1.0);

fn eq(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity("=", "2"));
    }
    if **args[0] == **args[1] {
        Ok(Value::boolean(true))
    } else {
        Ok(Value::boolean(false))
    }
}

macro_rules! construct_cmp {
    ($args:ident, $func:ident, $func_name:expr) => (
        match **$args[0] {
            Type::Int(fst) => {
                match **$args[1] {
                    Type::Float(snd) => if (fst as f64).$func(&snd) {
                        Ok(Value::boolean(true))
                    } else {
                        Ok(Value::boolean(false))
                    },
                    Type::Int(snd) => if fst.$func(&snd) {
                        Ok(Value::boolean(true))
                    } else {
                        Ok(Value::boolean(false))
                    },
                    _ => Err(Error::InvalidArg(stringify!($func_name),
                             "either integer or floating point numbers")),
                }
            },
            Type::Float(fst) => {
                match **$args[1] {
                    Type::Float(snd) => if fst.$func(&snd) {
                        Ok(Value::boolean(true))
                    } else {
                        Ok(Value::boolean(false))
                    },
                    Type::Int(snd) => if fst.$func(&(snd as f64)) {
                        Ok(Value::boolean(true))
                    } else {
                        Ok(Value::boolean(false))
                    },
                    _ => Err(Error::InvalidArg(stringify!($func_name),
                             "either integer or floating point numbers")),
                }
            },
            _ => Err(Error::InvalidArg(stringify!($func_name),
                     "either integer or floating point numbers")),
        }
    )
}

fn lt(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity("<", "2"));
    }
    construct_cmp!(args, lt, "<")
}

fn le(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity("<=", "2"));
    }
    construct_cmp!(args, le, "<=")
}

fn gt(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity(">", "2"));
    }
    construct_cmp!(args, gt, ">")
}

fn ge(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity(">=", "2"));
    }
    construct_cmp!(args, ge, ">=")
}

// predicates
fn atomq(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("atom?", "1"));
    }
    if let Type::Atom(_) = **args[0] {
        Ok(Value::boolean(true))
    } else {
        Ok(Value::boolean(false))
    }
}

fn emptyq(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("empty?", "1"));
    }
    match **args[0] {
        Type::List(ref v) | Type::Vector(ref v) => {
            if v.len() == 0 {
                Ok(Value::boolean(true))
            } else {
                Ok(Value::boolean(false))
            }
        }
        _ => Err(Error::InvalidArg("empty?", "either list or vector")),
    }
}

fn falseq(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("false?", "1"));
    }
    if let Type::Boolean(b) = **args[0] {
        Ok(Value::boolean(!b))
    } else {
        Err(Error::InvalidArg("false?", "boolean value"))
    }
}

fn keywordq(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("keyword?", "1"));
    }
    if let Type::Keyword(_) = **args[0] {
        Ok(Value::boolean(true))
    } else {
        Err(Error::InvalidArg("keyword?", "any value"))
    }
}

fn listq(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("list?", "1"));
    }
    if let Type::List(_) = **args[0] {
        Ok(Value::boolean(true))
    } else {
        Ok(Value::boolean(false))
    }
}

fn mapq(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("map?", "1"));
    }
    if let Type::Map(_) = **args[0] {
        Ok(Value::boolean(true))
    } else {
        Err(Error::InvalidArg("map?", "any value"))
    }
}

fn seqq(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("seq?", "1"));
    }
    match **args[0] {
        Type::Vector(_) | Type::List(_) => Ok(Value::boolean(true)),
        _ => Ok(Value::boolean(false)),
    }
}

fn stringq(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("string?", "1"));
    }
    if let Type::Str(_) = **args[0] {
        Ok(Value::boolean(true))
    } else {
        Ok(Value::boolean(false))
    }
}

fn symbolq(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("symbol?", "1"));
    }
    if let Type::Symbol(_) = **args[0] {
        Ok(Value::boolean(true))
    } else {
        Err(Error::InvalidArg("symbol?", "any value"))
    }
}

fn trueq(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("true?", "1"));
    }
    if let Type::Boolean(b) = **args[0] {
        Ok(Value::boolean(b))
    } else {
        Err(Error::InvalidArg("true?", "boolean value"))
    }
}

fn vectorq(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("vector?", "1"));
    }
    if let Type::Vector(_) = **args[0] {
        Ok(Value::boolean(true))
    } else {
        Ok(Value::boolean(false))
    }
}


pub fn standart_environment() -> Environment {
    let result = EnvironmentStruct::new(None);
    {
        let mut r = result.borrow_mut();
        // atoms
        r.set("atom".into(), Value::func(atom));
        r.set("deref".into(), Value::func(deref));
        r.set("reset".into(), Value::func(reset));
        r.set("swap".into(), Value::func(swap));

        // construction
        r.set("keyword".into(), Value::func(keyword));
        r.set("str".into(), Value::func(str));
        r.set("symbol".into(), Value::func(symbol));
        r.set("vector".into(), Value::func(vector));

        // hash-maps
        r.set("assoc".into(), Value::func(assoc));
        r.set("contains?".into(), Value::func(containsq));
        r.set("dissoc".into(), Value::func(dissoc));
        r.set("get".into(), Value::func(get));
        r.set("hash-map".into(), Value::func(hash_map));
        r.set("keys".into(), Value::func(keys));
        r.set("values".into(), Value::func(values));

        // io
        r.set("print".into(), Value::func(print));
        r.set("println".into(), Value::func(println));
        r.set("read-string".into(), Value::func(read_string));
        r.set("slurp".into(), Value::func(slurp));

        // lists
        r.set("apply".into(), Value::func(apply));
        r.set("concat".into(), Value::func(concat));
        r.set("cons".into(), Value::func(cons));
        r.set("count".into(), Value::func(count));
        r.set("first".into(), Value::func(first));
        r.set("list".into(), Value::func(list));
        r.set("nth".into(), Value::func(nth));
        r.set("rest".into(), Value::func(rest));

        // math and ordering
        r.set("+".into(), Value::func(add));
        r.set("-".into(), Value::func(sub));
        r.set("*".into(), Value::func(mult));
        r.set("/".into(), Value::func(div));
        r.set("=".into(), Value::func(eq));
        r.set("<=".into(), Value::func(le));
        r.set("<".into(), Value::func(lt));
        r.set(">=".into(), Value::func(ge));
        r.set(">".into(), Value::func(gt));

        // predicates
        r.set("atom?".into(), Value::func(atomq));
        r.set("empty?".into(), Value::func(emptyq));
        r.set("false?".into(), Value::func(falseq));
        r.set("keyword?".into(), Value::func(keywordq));
        r.set("list?".into(), Value::func(listq));
        r.set("map?".into(), Value::func(mapq));
        r.set("seq?".into(), Value::func(seqq));
        r.set("string?".into(), Value::func(stringq));
        r.set("symbol?".into(), Value::func(symbolq));
        r.set("true?".into(), Value::func(trueq));
        r.set("vector?".into(), Value::func(vectorq));
    }
    result
}

pub fn read_eval(s: &str, env: Environment) -> LispResult {
    use Reader;
    use eval::eval;
    let ast = Reader::read(s.as_bytes())?;
    eval(ast, env)
}
