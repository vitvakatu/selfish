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

fn internal_slurp(args: LispList) -> LispResult {
    if args.len() != 1 {
        return Err("Invalid arity of 'slurp' function".to_owned());
    }
    use std::io::prelude::*;
    use std::fs::File;
    if let LispType::Str(ref s) = **args[0] {
        let mut f = File::open(s).unwrap();
        let mut buffer = String::new();
        f.read_to_string(&mut buffer).unwrap();
        Ok(LispValue::string(buffer))
    } else {
        Err("Invalid argument of 'slurp' function (should be string)".to_owned())
    }
}

pub fn internal_atom(args: LispList) -> LispResult {
    if args.len() != 1 {
        return Err("Invalid arity of 'atom' function".to_owned());
    }
    let t = args[0].clone();
    Ok(LispValue::atom(t))
}

pub fn internal_atomq(args: LispList) -> LispResult {
    if args.len() != 1 {
        return Err("Invalid arity of 'atom?' function".to_owned());
    }
    if let LispType::Atom(_) = **args[0] {
        Ok(LispValue::boolean(true))
    } else {
        Ok(LispValue::boolean(false))
    }
}

pub fn internal_deref(args: LispList) -> LispResult {
    if args.len() != 1 {
        return Err("Invalid arity of 'deref' function".to_owned());
    }
    if let LispType::Atom(ref v) = **args[0] {
        Ok(v.borrow().clone())
    } else {
        Err("Invalid argument of 'deref' function (should be atom)".to_owned())
    }
}

pub fn internal_reset(args: LispList) -> LispResult {
    if args.len() != 2 {
        return Err("Invalid arity of 'reset!' function".to_owned());
    }
    if let LispType::Atom(ref v) = **args[0] {
        *v.borrow_mut() = args[1].clone();
        Ok(v.borrow().clone())
    } else {
        Err("Invalid arguments of 'reset!' function (should be atom and any)".to_owned())
    }
}

pub fn internal_swap(args: LispList) -> LispResult {
    if args.len() < 2 {
        return Err("Invalid arity of 'swap!' function".to_owned());
    }
    let mut func_args = args[2..].to_vec();
    if let LispType::Atom(ref v) = **args[0] {
        let val = v.borrow().clone();
        func_args.insert(0, val);
        match **args[1].clone() {
            LispType::Closure(ref closure) => {
                let new_env = EnvironmentStruct::with_bindings(
                    Some(closure.env.clone()),
                    closure.binds.clone(),
                    func_args
                );
                use eval::eval;
                let new_val = eval(closure.body.clone(), new_env.clone())?;
                *v.borrow_mut() = new_val.clone();
                Ok(new_val)
            },
            LispType::Func(func) => {
                let new_val = func(func_args)?;
                *v.borrow_mut() = new_val.clone();
                Ok(new_val)
            }
            _ => Err("Invalid argument: Not a closure or function".to_owned())
        }
    } else {
        Err("Not atom".to_owned())
    }
}

fn internal_cons(args: LispList) -> LispResult {
    if args.len() != 2 {
        return Err("Invalid arity of 'cons' function".to_owned());
    }
    if let LispType::List(ref v) = **args[1].clone() {
        let mut result = v.clone();
        result.insert(0, args[0].clone());
        Ok(LispValue::list(result))
    } else {
        Err("Invalid arguments of 'cons' function".to_owned())
    }
}

fn internal_concat(args: LispList) -> LispResult {
    let mut result = Vec::new();
    for e in args {
        match **e {
            LispType::List(ref v) => result.extend(v.iter().cloned()),
            _ => return Err("Invalid arguments of 'concat' function".to_owned()),
        }
    }
    Ok(LispValue::list(result))
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
        r.set("slurp".to_owned(), LispValue::func(internal_slurp));

        r.set("atom".to_owned(), LispValue::func(internal_atom));
        r.set("atom?".to_owned(), LispValue::func(internal_atomq));
        r.set("deref".to_owned(), LispValue::func(internal_deref));
        r.set("reset!".to_owned(), LispValue::func(internal_reset));
        r.set("swap!".to_owned(), LispValue::func(internal_swap));

        r.set("cons".to_owned(), LispValue::func(internal_cons));
        r.set("concat".to_owned(), LispValue::func(internal_concat));
    }
    result
}