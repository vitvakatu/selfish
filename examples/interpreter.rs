extern crate rustyline;
use rustyline::Editor;

extern crate mylisp;
use mylisp::*;

fn read(s: String) -> LispValue {
    Reader::read(s.as_bytes()).unwrap_or(LispValue::new(LispType::List(Vec::new())))
}

fn eval(s: LispValue) -> LispValue {
    s
}

fn print(s: LispValue) {
    println!("{}", &Writer::print(s));
}

fn main() {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                print(eval(read(line)));
            },
            Err(_) => break,
        }
    }
}