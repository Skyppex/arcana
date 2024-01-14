use std::io::{stdin, stdout, Write};

use shared::{type_checker::create_typed_ast, display::{Indent, IndentDisplay}, parser::create_ast};

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

        let program = create_ast(tokens)?;
        let mut indent = Indent::new();
        println!("{}\n", program.indent_display(&mut indent));

        let typed_program = create_typed_ast(program)?;
        let mut indent = Indent::new();
        println!("{}\n", typed_program.indent_display(&mut indent));
    }
}
