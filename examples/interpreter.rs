extern crate rustyline;
use rustyline::Editor;

extern crate mylisp;
use mylisp::*;

fn read(s: String) -> LispResult {
    Reader::read(s.as_bytes())
}

fn eval(s: LispValue) -> LispResult {
    Ok(s)
}

fn print(s: LispResult) {
    match s {
        Ok(v) => println!("{}", &Writer::print(v)),
        Err(e) => println!("Eval error: {}", &e),
    }
}

fn main() {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                match read(line) {
                    Ok(value) => print(eval(value)),
                    Err(e) => println!("Read error: {}", &e),
                }
            },
            Err(_) => break,
        }
    }
}