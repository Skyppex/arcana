mod interactive;
mod mage_args;
mod utils;

use clap::Parser;
use utils::get_path;

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
        crate::interactive::interactive(&args)
    };

    if let Err(error) = result {
        eprintln!("Fatal: {}", error);
    }
}

pub fn run_source(source: &String, args: &MageArgs) -> Result<(), String> {
    let source = get_path(source)
        .map_err(|e| e.to_string())?
        .to_str()
        .ok_or_else(|| "Failed to convert path to string".to_string())?
        .to_string();

    if !source.trim().ends_with(".ar") {
        return Err("Mage cannot cast a spell yet, it can only interperet arcana.".to_string());
    }

    let source = std::fs::read_to_string(source)
        .map_err(|error| format!("Failed to read file: {}", error))?;

    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new()));
    let environment = Rc::new(RefCell::new(Environment::new()));

    let result = read_input(source, type_environment.clone(), environment.clone(), args);

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
    args: &MageArgs,
) -> Result<(), String> {
    let print_tokens = args.logging.log_flags.tokens;
    let print_parser_ast = args.logging.log_flags.ast;
    let print_type_checker_ast = args.logging.log_flags.typed_ast;

    let tokens = shared::lexer::tokenize(&input)?;
    if print_tokens {
        eprintln!("{}\n", tokens.prettify());
    }

    let program = create_ast(tokens, args.logging.verbose)?;
    if print_parser_ast {
        eprintln!("{}\n", program.prettify());
    }

    let typed_program = create_typed_ast(program, type_environment)?;
    if print_type_checker_ast {
        eprintln!("{}\n", typed_program.prettify());
    }

    let result = interpreter::evaluate(typed_program, environment)?;

    if print_tokens | print_parser_ast || print_type_checker_ast {
        eprintln!("{}", input);
    }

    println!("{}", result);
    Ok(())
}
