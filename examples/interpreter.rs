extern crate rustyline;
use rustyline::Editor;

extern crate selfish;
use selfish::*;

fn read(s: String) -> LispResult {
    Reader::read(s.as_bytes())
}

fn print(s: LispResult) {
    match s {
        Ok(v) => println!("{}", &Writer::print(v, false)),
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