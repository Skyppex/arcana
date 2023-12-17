use std::io::{stdin, stdout, Write};

fn main() {
    loop {
        let _ = stdout().flush();
        let mut input = String::new();
        stdin().read_line(&mut input).expect("Failed to read line");

        if let "q" | "quit" | "exit" = input.trim() {
            break;
        }

        let tokens = shared::lexer::tokenize(input);
        println!("{:?}", tokens);
    }
}
