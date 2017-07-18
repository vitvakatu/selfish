use {LispResult, Error, Value, Type, Environment, EnvironmentStruct, Closure};

fn quasiquote(s: Value) -> Value {
    match **s.clone() {
        Type::List(ref v) if !v.is_empty() => {
            match **v[0].clone() {
                Type::Symbol(ref sym) if sym == "unquote" && v.len() >= 2 => {
                    return v[1].clone();
                }
                Type::List(ref v2) if v2.len() >= 2 => {
                    match **v2[0].clone() {
                        Type::Symbol(ref sym2) if sym2 == "splice-unquote" => {
                            return Value::list(vec![
                                Value::symbol("concat".to_owned()),
                                v2[1].clone(),
                                quasiquote(Value::list(v[1..].to_vec())),
                            ]);
                        }
                        _ => {
                            return Value::list(vec![
                                Value::symbol("cons".to_owned()),
                                quasiquote(v[0].clone()),
                                quasiquote(Value::list(v[1..].to_vec())),
                            ])
                        }
                    }
                }
                _ => {
                    return Value::list(vec![
                        Value::symbol("cons".to_owned()),
                        quasiquote(v[0].clone()),
                        quasiquote(Value::list(v[1..].to_vec())),
                    ]);
                }
            }
        }
        _ => return Value::list(vec![Value::symbol("quote".to_owned()), s]),
    }
}

fn is_macro_call(ast: &Value, env: Environment) -> Option<Closure> {
    if let Type::List(ref v) = ***ast {
        if let Type::Symbol(ref sym) = **v[0] {
            match env.borrow().get(sym.clone()) {
                Ok(val) => {
                    if let Type::Closure(ref c) = **val {
                        if c.is_macro {
                            return Some(c.clone());
                        }
                    }
                }
                Err(_) => return None,
            }
        }
    }
    None
}

fn macroexpand(mut ast: Value, mut env: Environment) -> LispResult {
    while let Some(closure) = is_macro_call(&ast, env.clone()) {
        match **ast.clone() {
            Type::List(ref v) => {
                let args = v[1..].to_vec();
                let new_env = EnvironmentStruct::with_bindings(
                    Some(closure.env.clone()),
                    closure.binds.clone(),
                    args,
                ).map_err(Error::BindError)?;
                ast = eval(closure.body.clone(), new_env.clone())?;
                env = new_env.clone();
                continue;
            }
            _ => unreachable!(),
        }
    }
    Ok(ast)
}

fn eval_ast(s: &Value, env: Environment) -> LispResult {
    match ***s {
        Type::Symbol(ref k) => env.borrow().get(k.clone()),
        Type::List(ref v) => {
            let mut result = Vec::new();
            for e in v {
                result.push(eval(e.clone(), env.clone())?);
            }
            Ok(Value::list(result))
        }
        Type::Vector(ref v) => {
            let mut result = Vec::new();
            for e in v {
                result.push(eval(e.clone(), env.clone())?);
            }
            Ok(Value::vector(result))
        }
        Type::Map(ref m) => {
            let mut result = m.clone();
            for (k, v) in m {
                result.insert(k.to_owned(), eval(v.clone(), env.clone())?);
            }
            Ok(Value::map(result))
        }
        _ => Ok(s.clone()),
    }
}

pub fn eval(mut s: Value, mut env: Environment) -> LispResult {
    loop {
        s = macroexpand(s.clone(), env.clone())?;
        match **s.clone() {
            Type::List(ref v) => {
                if v.is_empty() {
                    return Ok(s.clone());
                }
                match **v[0] {
                    Type::Symbol(ref sym) if sym == "def" => {
                        if v.len() != 3 {
                            return Err(Error::InvalidArity("def", "2"));
                        }
                        if let Type::Symbol(ref s) = **v[1] {
                            let val = eval(v[2].clone(), env.clone())?;
                            env.borrow_mut().set(s.clone(), val.clone());
                            return Ok(val.clone());
                        } else {
                            return Err(Error::InvalidArg("def", "symbol followed by any value"));
                        }
                    }
                    Type::Symbol(ref sym) if sym == "defmacro" => {
                        if v.len() != 4 {
                            return Err(Error::InvalidArity("defmacro!", "3"));
                        }
                        if let Type::Symbol(ref name) = **v[1] {
                            let closure = Value::list(
                                vec![Value::symbol("fn".into()), v[2].clone(), v[3].clone()],
                            );
                            let val = eval(closure, env.clone())?;
                            if let Type::Closure(ref closure) = **val {
                                let macros = Value::macros(closure.clone());
                                env.borrow_mut().set(name.clone(), macros.clone());
                                return Ok(macros.clone());
                            }
                        }
                        return Err(Error::InvalidArg(
                            "defmacro",
                            "symbol followed by list of symbols and any value",
                        ));
                    }
                    Type::Symbol(ref sym) if sym == "let" => {
                        if v.len() != 3 {
                            return Err(Error::InvalidArity("let", "2"));
                        }
                        let new_env = EnvironmentStruct::new(Some(env.clone()));
                        match **v[1] {
                            Type::Vector(ref v) | Type::List(ref v) => {
                                if v.len() % 2 != 0 {
                                    return Err(
                                        Error::Custom("Bindings in 'let' aren't balanced".into()),
                                    );
                                }
                                let mut it = v.iter();
                                while it.len() >= 2 {
                                    let bind = it.next().unwrap();
                                    let expr = it.next().unwrap();
                                    if let Type::Symbol(ref b) = ***bind {
                                        let e = eval(expr.clone(), new_env.clone())?;
                                        new_env.borrow_mut().set(b.clone(), e);
                                    }
                                }
                            }
                            _ => {
                                return Err(Error::InvalidArg(
                                    "let",
                                    "vector or list of pairs (symbol, any value),\
                                     followed by any value",
                                ))
                            }
                        }
                        s = v[2].clone();
                        env = new_env.clone();
                        continue;
                    }
                    Type::Symbol(ref sym) if sym == "do" => {
                        if v.len() == 1 {
                            return Err(Error::InvalidArity("do", ">= 1"));
                        }
                        let len = v.len() - 1;
                        for e in &v[1..len] {
                            let _ = eval(e.clone(), env.clone())?;
                        }
                        s = v[len].clone();
                        continue;
                    }
                    Type::Symbol(ref sym) if sym == "if" => {
                        if v.len() != 4 {
                            return Err(Error::InvalidArity("if", "3"));
                        }
                        let expr = eval(v[1].clone(), env.clone())?;
                        match **expr {
                            Type::List(ref vec) | Type::Vector(ref vec) if vec.is_empty() => {
                                s = v[3].clone();
                                continue;
                            }
                            Type::Boolean(ref b) if b == &false => {
                                s = v[3].clone();
                                continue;
                            }
                            _ => {
                                s = v[2].clone();
                                continue;
                            }
                        }
                    }
                    Type::Symbol(ref sym) if sym == "eval" => {
                        if v.len() != 2 {
                            return Err(Error::InvalidArity("eval", "1"));
                        }
                        let ast = v[1].clone();
                        s = eval(ast, env.clone())?;
                        env = EnvironmentStruct::top(&env);
                        continue;
                    }
                    Type::Symbol(ref sym) if sym == "quote" => {
                        if v.len() == 1 {
                            return Err(Error::InvalidArity("quote", "> 1"));
                        }
                        return Ok(v[1].clone());
                    }
                    Type::Symbol(ref sym) if sym == "quasiquote" => {
                        if v.len() == 1 {
                            return Err(Error::InvalidArity("quasiquote", "> 1"));
                        }
                        s = quasiquote(v[1].clone());
                        continue;
                    }
                    Type::Symbol(ref sym) if sym == "macroexpand" => {
                        if v.len() != 2 {
                            return Err(Error::InvalidArity("macroexpand", "1"));
                        }
                        return macroexpand(v[1].clone(), env.clone());
                    }
                    Type::Symbol(ref sym) if sym == "fn" => {
                        if v.len() != 3 {
                            return Err(Error::InvalidArity("fn", "2"));
                        }
                        let binds = match **v[1].clone() {
                            Type::List(ref v) => {
                                let mut result = Vec::new();
                                for e in v {
                                    if let Type::Symbol(ref sym) = ***e {
                                        result.push(sym.clone());
                                    } else {
                                        return Err(Error::InvalidArg(
                                            "fn",
                                            "list of symbols followed by any value",
                                        ));
                                    }
                                }
                                result
                            }
                            _ => {
                                return Err(Error::InvalidArg(
                                    "fn",
                                    "list of symbols followed by any value",
                                ))
                            }
                        };
                        let body = (**v[2]).clone();
                        return Ok(Value::closure(binds, Value::new(body), env.clone()));
                    }
                    Type::Symbol(ref sym) if sym == "throw" => {
                        if v.len() != 2 {
                            return Err(Error::InvalidArity("throw", "1"));
                        }
                        return Err(Error::Value(v[1].clone()));
                    }
                    Type::Symbol(ref sym) if sym == "try" => {
                        if v.len() != 3 {
                            return Err(Error::InvalidArity("try", "2"));
                        }
                        let err_val = match eval(v[1].clone(), env.clone()) {
                            ok @ Ok(_) => return ok,
                            Err(e) => {
                                match e {
                                    Error::Value(value) => value.clone(),
                                    any => Value::string(any.to_string()),
                                }
                            }
                        };
                        if let Type::List(ref list) = **v[2] {
                            if let Type::Symbol(ref sym) = **list[0] {
                                if sym != "catch" {
                                    return Err(Error::InvalidArg(
                                        "try",
                                        "any value followed by catch function",
                                    ));
                                }
                            }
                            if list.len() != 3 {
                                return Err(Error::InvalidArity("catch", "2"));
                            }
                            if let Type::Symbol(ref bind_sym) = **list[1] {
                                let new_env = EnvironmentStruct::with_bindings(
                                    Some(env.clone()),
                                    vec![bind_sym.clone()],
                                    vec![err_val],
                                ).map_err(Error::BindError)?;
                                s = list[2].clone();
                                env = new_env;
                                continue;
                            } else {
                                return Err(
                                    Error::InvalidArg("catch", "symbol followed by any value"),
                                );
                            }
                        }
                    }
                    _ => {
                        let evaluated = eval_ast(&s, env.clone())?;
                        match **evaluated {
                            Type::List(ref v) => {
                                let args = v[1..].to_vec();
                                match **v[0].clone() {
                                    Type::Func(func) => return func(args),
                                    Type::Closure(ref closure) => {
                                        let new_env = EnvironmentStruct::with_bindings(
                                            Some(closure.env.clone()),
                                            closure.binds.clone(),
                                            args,
                                        ).map_err(Error::BindError)?;
                                        s = closure.body.clone();
                                        env = new_env.clone();
                                        continue;
                                    }
                                    _ => {
                                        return Err(
                                            Error::Custom("Attempt to call not a funtion".into()),
                                        )
                                    }
                                }
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }
            _ => return eval_ast(&s, env),
        }
    }
}
