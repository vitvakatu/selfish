extern crate rustyline;
use rustyline::Editor;

extern crate selfish;
use selfish::*;

fn print(s: LispResult) {
    match s {
        Ok(v) => println!("{}", &Writer::print(v, false)),
        Err(e) => println!("Eval error: {}", &e),
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
    loop {
        let readline = rl.readline(">> ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                print(read_eval(line, environment.clone()));
            },
            Err(_) => break,
        }
    }
}