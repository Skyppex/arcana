mod interpreter;

use std::{cell::RefCell, fs::{self}, io::{stdin, stdout, Write}, path::Path, rc::Rc};

use interpreter::environment::Environment;
use shared::{type_checker::{create_typed_ast, TypeEnvironment}, display::{Indent, IndentDisplay}, parser::create_ast};

fn main() {
    let result = run_program();

    if let Err(error) = result {
        println!("Fatal: {}", error);
    }
}

fn run_program() -> Result<(), String> {
    let mut type_environemnt = TypeEnvironment::new();
    let environment = Rc::new(RefCell::new(Environment::new()));

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
            for (.., type_) in type_environemnt.get_types() {
                println!("{}", type_);
            }
            continue;
        }

        if let "vars" = input.trim() {
            for (.., variable) in type_environemnt.get_variables() {
                println!("{}", variable);
            }
            continue;
        }

        if let Err(message) = read_input(input, &mut type_environemnt, environment.clone()) {
            println!("Error: {}", message);
        }
    }
}

fn read_input(
    input: String,
    type_environemnt: &mut TypeEnvironment<'_>,
    environment: Rc<RefCell<Environment>>) -> Result<(), String> {
    const PRINT_TOKENS: bool = false;
    const PRINT_PARSER_AST: bool = true;
    const PRINT_TYPE_CHECKER_AST: bool = true;

    let tokens = shared::lexer::tokenize(&input)?;
    if PRINT_TOKENS {
        println!("{:?}\n", tokens);
    }

    let program = create_ast(tokens)?;
    if PRINT_PARSER_AST {
        let mut indent = Indent::new();
        println!("{}\n", program.indent_display(&mut indent));
    }

    let typed_program = create_typed_ast(program, type_environemnt)?;
    if PRINT_TYPE_CHECKER_AST {
        let mut indent = Indent::new();
        println!("{}\n", typed_program.indent_display(&mut indent));
    }

    let result = interpreter::evaluate(
        typed_program,
        environment)?;

    if PRINT_TOKENS | PRINT_PARSER_AST || PRINT_TYPE_CHECKER_AST {
        println!("{}", input);
    }

    println!("{}\n", result);
    Ok(())
}
