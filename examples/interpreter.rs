extern crate rustyline;
use rustyline::Editor;

extern crate selfish;
use selfish::*;

fn print(s: LispResult) {
    match s {
        Ok(v) => println!("{}", &Writer::print(v, false)),
        Err(e) => println!("Error: {}", &e),
    }
}

fn main() {
    let mut rl = Editor::<()>::new();
    let environment = standart_environment();
    println!("Loading prelude...");
    match read_eval("(load-file \"prelude.slf\")".into(), environment.clone()) {
        Ok(_) => println!("Done"),
        Err(e) => println!("Error, you have access to basic functions only\n\
                            Reason: {}", &e),
    }
    let mut incomplete = false;
    let mut previous_input = String::new();
    loop {
        let readline = rl.readline(">> ");

        match readline {
            Ok(line) => {
                if incomplete {
                    previous_input.push_str("\n");
                    previous_input.push_str(&line);
                }
                let parse_result = if incomplete {
                    rl.add_history_entry(&previous_input);
                    read_eval(&previous_input, environment.clone())
                } else {
                    rl.add_history_entry(&line);
                    read_eval(&line, environment.clone())
                };
                match parse_result {
                    a @ Ok(_) => {
                        incomplete = false;
                        previous_input.clear();
                        print(a);
                    },
                    Err(e) => match e {
                        Error::Incomplete => {
                            if !incomplete {
                                previous_input.push_str(&line);
                            }
                            incomplete = true;
                        },
                        _ => {
                            incomplete = false;
                            previous_input.clear();
                            print(Err(e));
                        },
                    }
                }
            },
            Err(_) => break,
        }
    }
}