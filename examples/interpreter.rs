extern crate rustyline;
use rustyline::Editor;

extern crate mylisp;
use mylisp::*;


use std::rc::Rc;

fn read(s: String) -> LispResult {
    Reader::read(s.as_bytes())
}

fn eval_ast(s: LispValue, env: Environment) -> LispResult {
    match *s {
        LispType::Symbol(ref k) => env.borrow().get(k.clone()),
        LispType::List(ref v) | LispType::Vector(ref v) => {
            let mut result = Vec::new();
            for e in v {
                result.push(eval(e.clone(), env.clone())?);
            }
            Ok(Rc::new(LispType::List(result)))
        },
        LispType::Map(ref m) => {
            let mut result = m.clone();
            for (k, v) in m {
                result.insert(k.to_owned(), eval(v.clone(), env.clone())?);
            }
            Ok(Rc::new(LispType::Map(result)))
        },
        _ => Ok(s.clone()),
    }
}

fn eval(s: LispValue, env: Environment) -> LispResult {
    match *s {
        LispType::List(ref v) => {
            if v.len() == 0 {
                return Ok(s.clone());
            }
            match *v[0] {
                LispType::Symbol(ref sym) if sym == "def!" => {
                    if v.len() != 3 {
                        return Err("Wrong arity of 'def!'".to_owned());
                    }
                    if let LispType::Symbol(ref s) = *v[1] {
                        let val = eval(v[2].clone(), env.clone())?;
                        env.borrow_mut().set(s.clone(), val.clone());
                        return Ok(val.clone())
                    } else {
                        return Err("First arg of 'def!' should be symbol".to_owned());
                    }
                },
                LispType::Symbol(ref sym) if sym == "let" => {
                    if v.len() != 3 {
                        return Err("Wrong arity of 'let'!".to_owned());
                    }
                    let new_env = EnvironmentStruct::new(Some(env.clone()));
                    if let LispType::Vector(ref v) = *v[1] {
                        if v.len() % 2 != 0 {
                            return Err("Count of the 'let' bindings isn't even".to_owned());
                        }
                        let mut it = v.iter();
                        while it.len() >= 2 {
                            let bind = it.next().unwrap();
                            let expr = it.next().unwrap();
                            if let LispType::Symbol(ref b) = **bind {
                                let e = eval(expr.clone(), new_env.clone())?;
                                new_env.borrow_mut().set(b.clone(),e);
                            }
                        }
                    } else {
                        return Err("binding list must be vector".to_owned());
                    }
                    return eval(v[2].clone(), new_env.clone());
                },
                LispType::Symbol(ref sym) if sym == "do" => {
                    if v.len() <= 2 {
                        return Err("Empty do statement".to_owned());
                    }
                    let len = v.len() - 1;
                    for e in &v[1..len] {
                        let _ = eval(e.clone(), env.clone());
                    }
                    return eval(v[len].clone(), env.clone());
                },
                LispType::Symbol(ref sym) if sym == "if" => {
                    if v.len() != 4 {
                        return Err("Invalid arity of 'if' statement".to_owned());
                    }
                    let expr = eval(v[1].clone(), env.clone())?;
                    match *expr {
                        LispType::List(ref vec) |
                        LispType::Vector(ref vec) if vec.len() == 0 =>
                            return eval(v[3].clone(), env.clone()),
                        LispType::Boolean(ref b) if b == &false =>
                            return eval(v[3].clone(), env.clone()),
                        _ => return eval(v[2].clone(), env.clone()),
                    }
                },
                LispType::Symbol(ref sym) if sym == "fn" => {
                    if v.len() != 3 {
                        return Err("Invalid arity of 'fn' statement".to_owned());
                    }
                    let binds = match *v[1].clone() {
                        LispType::List(ref v) => {
                            let mut result = Vec::new();
                            for e in v {
                                if let LispType::Symbol(ref sym) = **e {
                                    result.push(sym.clone());
                                } else {
                                    return Err("Invalid parameters".to_owned());
                                }
                            }
                            result
                        },
                        _ => return Err("Invalid parameters".to_owned()),
                    };
                    //let body = v[2].clone();
                    /*let closure = |body: LispList, args: LispList, binds: Vec<String>, env: Environment| -> LispResult {
                        let new_env = EnvironmentStruct::with_bindings(Some(env.clone()), binds, args);
                        eval(body, new_env.clone())
                    };*/
                    let body = (*v[2]).clone();
                    let lisp_closure = LispClosure {
                        //func: Rc::new(closure),
                        binds: binds,
                        env: env.clone(),
                        body: Rc::new(body),
                    };
                    Ok(Rc::new(LispType::Closure(lisp_closure)))
                }
                _ => {
                    let evaluated = eval_ast(s.clone(), env.clone())?;
                    match *evaluated {
                        LispType::List(ref v) => {
                            let args = v[1..].to_vec();
                            match *v[0].clone() {
                                LispType::Func(func) => func(args),
                                LispType::Closure(ref closure) => {
                                    let new_env = EnvironmentStruct::with_bindings(
                                        Some(closure.env.clone()),
                                        closure.binds.clone(),
                                        args
                                    );
                                    eval(closure.body.clone(), new_env.clone())
                                }
                                _ => return Err("Not a function!".to_owned())
                            }
                            },
                        _ => unreachable!(),
                    }
                },
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
                    Ok(value) => print(eval(value, environment.clone())),
                    Err(e) => println!("Read error: {}", &e),
                }
            },
            Err(_) => break,
        }
    }
}