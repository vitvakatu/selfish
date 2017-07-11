use {List, LispResult, Error, Value, Type, Writer, Environment, EnvironmentStruct};

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

fn str(args: List) -> LispResult {
    let mut result = String::new();
    for e in args {
        result.push_str(&Writer::print(e.clone(), true));
    }
    Ok(Value::string(result))
}

fn list(args: List) -> LispResult {
    let mut result = Vec::new();
    for e in args {
        result.push(e.clone());
    }
    Ok(Value::list(result))
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
        },
        _ => Err(Error::InvalidArg("empty?", "either list or vector")),
    }
}

fn count(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("count", "1"));
    }
    match **args[0] {
        Type::List(ref v) | Type::Vector(ref v) => {
            Ok(Value::int(v.len() as isize))
        },
        _ => Err(Error::InvalidArg("count", "either list or vector")),
    }
}

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
                    _ => Err(Error::InvalidArg(stringify!($func_name), "either integer or floating point numbers")),
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
                    _ => Err(Error::InvalidArg(stringify!($func_name), "either integer or floating point numbers")),
                }
            },
            _ => Err(Error::InvalidArg(stringify!($func_name), "either integer or floating point numbers")),
        }
    )
}

fn lt(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity("<", "2"))
    }
    construct_cmp!(args, lt, "<")
}

fn le(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity("<=", "2"))
    }
    construct_cmp!(args, le, "<=")
}

fn gt(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity(">", "2"))
    }
    construct_cmp!(args, gt, ">")
}

fn ge(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity(">=", "2"))
    }
    construct_cmp!(args, ge, ">=")
}

fn read_string(args: List) -> LispResult {
    use Reader;
    if args.len() != 1 {
        return Err(Error::InvalidArity("read-string", "1"))
    }
    if let Type::Str(ref s) = **args[0] {
        Reader::read(s.as_bytes())
    } else {
        Err(Error::InvalidArg("read-string", "string"))
    }
}

fn slurp(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("slurp", "1"))
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

fn atom(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("atom", "1"))
    }
    let t = args[0].clone();
    Ok(Value::atom(t))
}

fn atomq(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("atom?", "1"))
    }
    if let Type::Atom(_) = **args[0] {
        Ok(Value::boolean(true))
    } else {
        Ok(Value::boolean(false))
    }
}

fn deref(args: List) -> LispResult {
    if args.len() != 1 {
        return Err(Error::InvalidArity("deref", "1"))
    }
    if let Type::Atom(ref v) = **args[0] {
        Ok(v.borrow().clone())
    } else {
        Err(Error::InvalidArg("deref", "atom"))
    }
}

fn reset(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity("reset!", "2"))
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
        return Err(Error::InvalidArity("swap!", ">= 2"))
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
                    func_args
                );
                use eval::eval;
                let new_val = eval(closure.body.clone(), new_env.clone())?;
                *v.borrow_mut() = new_val.clone();
                Ok(new_val)
            },
            Type::Func(func) => {
                let new_val = func(func_args)?;
                *v.borrow_mut() = new_val.clone();
                Ok(new_val)
            }
            _ => Err(Error::InvalidArg("swap!", "atom, either closure or function, any values"))
        }
    } else {
        Err(Error::InvalidArg("swap!", "atom, either closure or function, any values"))
    }
}

fn cons(args: List) -> LispResult {
    if args.len() != 2 {
        return Err(Error::InvalidArity("cons", "2"))
    }
    if let Type::List(ref v) = **args[1].clone() {
        let mut result = v.clone();
        result.insert(0, args[0].clone());
        Ok(Value::list(result))
    } else {
        Err(Error::InvalidArg("cons", "any value and list"))
    }
}

fn concat(args: List) -> LispResult {
    let mut result = Vec::new();
    for e in args {
        match **e {
            Type::List(ref v) => result.extend(v.iter().cloned()),
            _ => return Err(Error::InvalidArg("concat", "any amount of lists")),
        }
    }
    Ok(Value::list(result))
}

pub fn standart_environment() -> Environment {
    let result = EnvironmentStruct::new(None);
    {
        let mut r = result.borrow_mut();
        r.set("+".to_owned(), Value::func(add));
        r.set("-".to_owned(), Value::func(sub));
        r.set("*".to_owned(), Value::func(mult));
        r.set("/".to_owned(), Value::func(div));

        r.set("print".to_owned(), Value::func(print));
        r.set("println".to_owned(), Value::func(println));
        r.set("str".to_owned(), Value::func(str));
        r.set("list".to_owned(), Value::func(list));
        r.set("list?".to_owned(), Value::func(listq));
        r.set("empty?".to_owned(), Value::func(emptyq));
        r.set("count".to_owned(), Value::func(count));
        r.set("=".to_owned(), Value::func(eq));
        r.set("<=".to_owned(), Value::func(le));
        r.set("<".to_owned(), Value::func(lt));
        r.set(">=".to_owned(), Value::func(ge));
        r.set(">".to_owned(), Value::func(gt));

        r.set("read-string".to_owned(), Value::func(read_string));
        r.set("slurp".to_owned(), Value::func(slurp));

        r.set("atom".to_owned(), Value::func(atom));
        r.set("atom?".to_owned(), Value::func(atomq));
        r.set("deref".to_owned(), Value::func(deref));
        r.set("reset!".to_owned(), Value::func(reset));
        r.set("swap!".to_owned(), Value::func(swap));

        r.set("cons".to_owned(), Value::func(cons));
        r.set("concat".to_owned(), Value::func(concat));
    }
    let load_file = "(def! load-file (fn (f) (eval (read-string (slurp f)))))".into();
    read_eval(load_file, result.clone()).unwrap();
    result
}

pub fn read_eval(s: String, env: Environment) -> LispResult {
    use Reader;
    use eval::eval;
    let ast = Reader::read(s.as_bytes())?;
    eval(ast, env)
}