mod interactive;
mod mage_args;

use clap::Parser;

use std::{cell::RefCell, rc::Rc};

use crate::mage_args::MageArgs;
use interpreter::Environment;

use shared::{
    parser::create_ast,
    pretty_print::PrettyPrint,
    type_checker::{create_typed_ast, TypeEnvironment},
};

fn main() {
    let args = crate::mage_args::MageArgs::parse();

    let result = if let Some(ref source) = args.source {
        run_source(source, &args)
    } else {
        crate::interactive::interactive()
    };

    if let Err(error) = result {
        eprintln!("Fatal: {}", error);
    }
}

pub fn run_source(source: &String, args: &MageArgs) -> Result<(), String> {
    let source = std::fs::read_to_string(source)
        .map_err(|error| format!("Failed to read file: {}", error))?;

    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new()));
    let environment = Rc::new(RefCell::new(Environment::new()));

    let result = read_input(source, type_environment.clone(), environment.clone());

    if args.variables {
        if args.label {
            println!("// Variables:");
        }

        for (name, variable) in environment.borrow().get_variables() {
            println!("{}: {}", name, variable.clone().borrow().value);
        }
    }

    if args.variables && args.types {
        println!();
    }

    if args.types {
        if args.label {
            println!("// Types:");
        }

        for (.., type_) in type_environment.borrow().get_types() {
            println!("{}", type_);
        }
    }

    result
}

pub fn read_input(
    input: String,
    type_environment: Rc<RefCell<TypeEnvironment>>,
    environment: Rc<RefCell<Environment>>,
) -> Result<(), String> {
    const PRINT_TOKENS: bool = true;
    const PRINT_PARSER_AST: bool = true;
    const PRINT_TYPE_CHECKER_AST: bool = true;

    let tokens = shared::lexer::tokenize(&input)?;
    if PRINT_TOKENS {
        eprintln!("{}\n", tokens.prettify());
    }

    let program = create_ast(tokens)?;
    if PRINT_PARSER_AST {
        eprintln!("{}\n", program.prettify());
    }

    let typed_program = create_typed_ast(program, type_environment)?;
    if PRINT_TYPE_CHECKER_AST {
        eprintln!("{}\n", typed_program.prettify());
    }

    let result = interpreter::evaluate(typed_program, environment)?;

    if PRINT_TOKENS | PRINT_PARSER_AST || PRINT_TYPE_CHECKER_AST {
        eprintln!("{}", input);
    }

    println!("{}", result);
    Ok(())
}
