mod interactive;
mod mage_args;
mod utils;

use clap::Parser;
use glob::{glob, Paths};
use utils::get_path;

use std::{cell::RefCell, fs, rc::Rc, thread};

use crate::mage_args::MageArgs;
use interpreter::Environment;

use shared::{
    parser::{self, create_ast},
    pretty_print::PrettyPrint,
    type_checker::{create_typed_ast, TypeEnvironment},
};

const STACK_SIZE: usize = 4 * 1024 * 1024;

fn main() -> std::io::Result<()> {
    // Spawn thread with explicit stack size
    let child = thread::Builder::new().stack_size(STACK_SIZE).spawn(run)?;

    // Wait for thread to join
    child.join().unwrap();
    Ok(())
}

fn run() {
    let args = crate::mage_args::MageArgs::parse();

    let spell = get_path("spell.toml").map_err(|e| e.to_string()).ok();

    let mut spell_toml = None;
    let mut project_files = None;
    if let Some(s) = spell {
        spell_toml = std::fs::read_to_string(s).ok();
        project_files = glob("*.ar").ok();
    }

    let result = if let Some(ref source) = args.source {
        run_source(source, spell_toml, project_files, &args)
    } else {
        crate::interactive::interactive(&args)
    };

    if let Err(error) = result {
        eprintln!("Fatal: {}", error);
    }
}

pub fn run_source(
    source: &str,
    spell: Option<String>,
    project_files: Option<Paths>,
    args: &MageArgs,
) -> Result<(), String> {
    let source = get_path(source)
        .map_err(|e| e.to_string())?
        .to_str()
        .ok_or_else(|| "Failed to convert path to string".to_string())?
        .to_string();

    if !source.trim().ends_with(".ar") {
        return Err("Mage cannot cast a spell yet, it can only interpret arcana.".to_string());
    }

    let source = std::fs::read_to_string(source)
        .map_err(|error| format!("Failed to read file: {}", error))?;

    let exe = std::env::current_exe()
        .and_then(fs::canonicalize)
        .expect("Failed to get current executable")
        .parent()
        .expect("Failed to get parent directory of executable")
        .parent()
        .expect("Failed to get parent directory of executable")
        .parent()
        .expect("Failed to get parent directory of executable")
        .to_str()
        .expect("Failed to convert path to string")
        .to_string();

    let lib_path = format!("{}\\lib\\lib.ar", exe).replace('\\', "/");

    let lib = get_path(&lib_path)
        .map_err(|e| e.to_string())?
        .to_str()
        .ok_or_else(|| "Failed to convert path to string".to_string())?
        .to_string();

    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new(
        args.behavior.override_types,
    )));

    let environment = Rc::new(RefCell::new(Environment::new()));

    if let (Some(_), Some(project_files)) = (spell, project_files) {
        register_modules(project_files, type_environment.clone(), environment.clone())?;
    }

    if let Ok(lib) = std::fs::read_to_string(lib) {
        read_input(
            lib,
            type_environment.clone(),
            environment.clone(),
            args,
            false,
        )?
    }

    let result = read_input(
        source,
        type_environment.clone(),
        environment.clone(),
        args,
        true,
    );

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
    print_result: bool,
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

    if print_result {
        println!("{}", result);
    }

    Ok(())
}

pub fn register_modules(
    project_files: Paths,
    type_environment: Rc<RefCell<TypeEnvironment>>,
    environment: Rc<RefCell<Environment>>,
) -> Result<(), String> {
    for project_file in project_files {
        let project_file = project_file.map_err(|e| e.to_string())?;

        let source = std::fs::read_to_string(project_file)
            .map_err(|error| format!("Failed to read file: {}", error))?;

        let module_info = parser::discover_module(shared::lexer::tokenize(&source)?)?;

        if let Some((_, module_path)) = module_info {
            type_environment
                .borrow_mut()
                .add_module(module_path.clone());

            environment.borrow_mut().add_module(module_path);
        }
    }

    todo!()
}
