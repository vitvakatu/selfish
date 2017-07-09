use {LispList, LispResult, LispValue, LispType, Writer, Environment, EnvironmentStruct};

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

use std::ops::{Add, Sub, Div, Mul};
arithmetic_function!(add, Add::add, 0.0);
arithmetic_function!(sub, Sub::sub, 0.0);
arithmetic_function!(mult, Mul::mul, 1.0);
arithmetic_function!(div, Div::div, 1.0);

fn internal_print(args: LispList) -> LispResult {
    if args.len() != 1 {
        return Err("Invalid arity of 'print' function".to_owned());
    }
    print!("{}", &Writer::print(args[0].clone(), true));
    Ok(LispValue::nothing())
}

fn internal_println(args: LispList) -> LispResult {
    if args.len() != 1 {
        return Err("Invalid arity of 'print' function".to_owned());
    }
    println!("{}", &Writer::print(args[0].clone(), true));
    Ok(LispValue::nothing())
}

fn internal_str(args: LispList) -> LispResult {
    let mut result = String::new();
    for e in args {
        result.push_str(&Writer::print(e.clone(), true));
    }
    Ok(LispValue::string(result))
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

fn internal_read_string(args: LispList) -> LispResult {
    use Reader;
    if args.len() != 1 {
        return Err("Invalid arity of 'read-string' function".to_owned());
    }
    if let LispType::Str(ref s) = **args[0] {
        Reader::read(s.as_bytes())
    } else {
        Err("Invalid argument of 'read-string' function (should be string)".to_owned())
    }
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
        r.set("println".to_owned(), LispValue::func(internal_println));
        r.set("str".to_owned(), LispValue::func(internal_str));
        r.set("list".to_owned(), LispValue::func(internal_list));
        r.set("list?".to_owned(), LispValue::func(internal_listq));
        r.set("empty?".to_owned(), LispValue::func(internal_emptyq));
        r.set("count".to_owned(), LispValue::func(internal_count));
        r.set("=".to_owned(), LispValue::func(internal_eq));
        r.set("<=".to_owned(), LispValue::func(internal_le));
        r.set("<".to_owned(), LispValue::func(internal_lt));
        r.set(">=".to_owned(), LispValue::func(internal_ge));
        r.set(">".to_owned(), LispValue::func(internal_gt));

        r.set("read-string".to_owned(), LispValue::func(internal_read_string));
    }
    result
}