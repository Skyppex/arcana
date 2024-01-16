mod interpreter;

use std::{io::{stdin, stdout, Write}, fs::{self}, path::Path, collections::BinaryHeap};

use interpreter::evironment::Environment;
use shared::{type_checker::{create_typed_ast, TypeEnvironment}, display::{Indent, IndentDisplay}, parser::create_ast};

fn main() {
    let result = run_program();

    if let Err(error) = result {
        println!("Error: {}", error);
    }
}

fn run_program() -> Result<(), String> {
    let mut type_environemnt = TypeEnvironment::new();
    let mut environment = Environment::new();

    loop {
        let _ = stdout().flush();
        let mut input = String::new();
        stdin().read_line(&mut input).expect("Failed to read line");

        if let "q" | "quit" | "exit" = input.trim() {
            break Ok(());
        }

        if let "read" = input.trim() {
            let path = Path::new("src/mage/manual_testing/test.ar");
            match fs::read_to_string(path) {
                Ok(source) => input = source,
                Err(e) => {
                    println!("Failed to read file: {}", e);
                    continue;
                }
            }
        }

        if let "types" = input.trim() {
            for type_ in type_environemnt.get_types() {
                println!("{}", type_.1);
            }
            continue;
        }

        if let "vars" = input.trim() {
            for variable in type_environemnt.get_variables() {
                println!("{}", variable.1);
            }
            continue;
        }

        let tokens = shared::lexer::tokenize(&input)?;
        // println!("\n{:?}\n", tokens);

        let program = create_ast(tokens)?;
        let mut indent = Indent::new();
        println!("{}", program.indent_display(&mut indent));

        let typed_program = create_typed_ast(program, &mut type_environemnt)?;
        let mut indent = Indent::new();
        println!("{}\n", typed_program.indent_display(&mut indent));
        
        // for type_ in type_environemnt.get_types()
        //     .iter()
        //     .map(|(name, ..)| name)
        //     .collect::<BinaryHeap<&String>>() {
        //     println!("{}", type_);
        // }

        // for variable in type_environemnt.get_variables() {
        //     println!("{}", variable.0);
        // }

        let result = interpreter::evaluate(
            typed_program,
            &mut environment)?;

        println!("{}", input);
        println!("{}\n", result);
    }
}
