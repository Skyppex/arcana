use std::io::{stdin, stdout, Write};

use shared::parser::IndentDisplay;

fn main() {
    let result = run_program();

    if let Err(error) = result {
        println!("Error: {}", error);
    }
}

fn run_program() -> Result<(), String> {
    loop {
        let _ = stdout().flush();
        let mut input = String::new();
        stdin().read_line(&mut input).expect("Failed to read line");

        if let "q" | "quit" | "exit" = input.trim() {
            break Ok(());
        }

        let tokens = shared::lexer::tokenize(input)?;
        println!("\n{:?}\n", tokens);

        let program = shared::parser::create_ast(tokens)?;
        let mut indent = shared::parser::Indent::new();
        println!("{}", program.indent_display(&mut indent));
    }
}
