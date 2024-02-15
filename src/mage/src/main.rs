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
    let type_environemnt = Rc::new(RefCell::new(TypeEnvironment::new()));
    let environment = Rc::new(RefCell::new(Environment::new()));

    loop {
        let type_environemnt = type_environemnt.clone();
        let _ = stdout().flush();
        let mut input = String::new();
        stdin().read_line(&mut input).expect("Failed to read line");

        if let "q" | "quit" | "exit" = input.trim() {
            break Ok(());
        }

        if input.trim().starts_with("read") {
            let path = Path::new("src/mage/manual_testing");

            let file_name = input
                .split(" ")
                .skip(1)
                .collect::<Vec<&str>>()
                .join(" ")
                .replace("\r", "")
                .replace("\n", "");

            let path = path.join(file_name)
                .with_extension("ar");

            println!("Reading file: {:?}", path);

            match fs::read_to_string(path) {
                Ok(source) => input = source,
                Err(e) => {
                    println!("Failed to read file: {}", e);
                    continue;
                }
            }
        }

        if let "types" = input.trim() {
            for (.., type_) in type_environemnt.borrow().get_types() {
                println!("{}", type_);
            }
            continue;
        }

        if let "vars" = input.trim() {
            for (.., variable) in type_environemnt.borrow().get_variables() {
                println!("{}", variable);
            }
            continue;
        }

        if let Err(message) = read_input(input, type_environemnt, environment.clone()) {
            println!("Error: {}", message);
        }
    }
}

fn read_input(
    input: String,
    type_environemnt: Rc<RefCell<TypeEnvironment>>,
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
