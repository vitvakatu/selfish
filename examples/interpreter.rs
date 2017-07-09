extern crate rustyline;
use rustyline::Editor;

extern crate mylisp;
use mylisp::*;
use std::collections::HashMap;
use std::rc::Rc;

fn read(s: String) -> LispResult {
    Reader::read(s.as_bytes())
}

fn eval_ast(s: LispValue, env: &HashMap<String, LispValue>) -> LispResult {
    match *s {
        LispType::Symbol(ref k) => {
            match env.get(k) {
                Some(v) => Ok(v.clone()),
                None => Err(format!("Invalid symbol: {}", k)),
            }
        },
        LispType::List(ref v) | LispType::Vector(ref v) => {
            let mut result = Vec::new();
            for e in v {
                result.push(eval(e.clone(), env)?);
            }
            Ok(Rc::new(LispType::List(result)))
        },
        LispType::Map(ref m) => {
            let mut result = m.clone();
            for (k, v) in m {
                result.insert(k.to_owned(), eval(v.clone(), env)?);
            }
            Ok(Rc::new(LispType::Map(result)))
        },
        _ => Ok(s.clone()),
    }
}

fn eval(s: LispValue, env: &HashMap<String, LispValue>) -> LispResult {
    match *s {
        LispType::List(ref v) => {
            if v.len() == 0 {
                return Ok(s.clone());
            }
            let evaluated = eval_ast(s.clone(), env)?;
            match *evaluated {
                LispType::List(ref v) => {
                    let f = match *v[0].clone() {
                        LispType::Func(func) => func,
                        _ => return Err("Not a function!".to_owned())
                    };
                    let args = v[1..].to_vec();
                    (f)(args)
                },
                _ => unreachable!(),
            }
        },
        _ => eval_ast(s.clone(), env)
    }
}

fn print(s: LispResult) {
    match s {
        Ok(v) => println!("{}", &Writer::print(v)),
        Err(e) => println!("Eval error: {}", &e),
    }
}

fn main() {
    let mut rl = Editor::<()>::new();
    let environment = standart_environment();
    loop {
        let readline = rl.readline(">> ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                match read(line) {
                    Ok(value) => print(eval(value, &environment)),
                    Err(e) => println!("Read error: {}", &e),
                }
            },
            Err(_) => break,
        }
    }
}