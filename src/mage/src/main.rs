mod cli;
mod config;
mod interactive;
mod utils;

use clap::Parser;
use config::SpellConfig;
use glob::glob;
use utils::{get_path, normalize_path};

use std::{
    cell::RefCell,
    fs,
    path::{Path, PathBuf},
    rc::Rc,
    thread,
};

use crate::cli::Cli;
use interpreter::Environment;

use shared::{
    parser::{self, create_ast},
    pretty_print::PrettyPrint,
    type_checker::{create_typed_ast, discover_user_defined_types, TypeEnvironment},
};

const STACK_SIZE: usize = 4 * 1024 * 1024;

fn main() -> std::io::Result<()> {
    // Spawn thread with explicit stack size
    let child = thread::Builder::new().stack_size(STACK_SIZE).spawn(run)?;

    // Wait for thread to join
    child.join().unwrap()?;
    Ok(())
}

fn run() -> std::io::Result<()> {
    let args = crate::cli::Cli::parse();

    let result = if let Some(ref source) = args.source {
        run_source(source, &args)
    } else {
        crate::interactive::interactive(&args)
    };

    if let Err(error) = result {
        eprintln!("Fatal: {}", error);
    }

    Ok(())
}

pub fn run_source(source: &str, args: &Cli) -> Result<(), String> {
    let source = get_path(source).map_err(|e| e.to_string())?;

    let glob_pattern = format!("{}/**/*.ar", source.to_string_lossy()).replace('\\', "/");
    let project_files = glob(&glob_pattern).ok();

    let project_files = project_files
        .map(|paths| {
            paths
                .into_iter()
                .map(|path| path.map(normalize_path))
                .collect::<Result<Vec<_>, _>>()
                .map_err(|e| e.to_string())
        })
        .transpose()?;

    if source.is_dir() {
        let spell = source.join("spell.toml");

        if !spell.exists() {
            return Err("spell.toml not found".to_string());
        }

        let Some(project_files) = project_files else {
            return Err("No files with extension .ar found in workspace".to_string());
        };

        let spell_content = std::fs::read_to_string(spell).map_err(|e| format!("{}", e))?;

        let spell_config = toml::from_str::<SpellConfig>(&spell_content)
            .map_err(|e| format!("Failed to parse spell.toml: {}", e))?;

        return run_spell(spell_config, project_files, &source, args);
    }

    if source.extension().is_none_or(|ext| ext != "ar") {
        return Err("source must be a folder or a file with the .ar extension".to_string());
    }

    run_script(source, project_files, args)
}

fn run_spell(
    spell: SpellConfig,
    project_files: Vec<PathBuf>,
    source: &Path,
    args: &Cli,
) -> Result<(), String> {
    let main = spell
        .main
        .map(|m| get_path(&m))
        .transpose()
        .map_err(|e| e.to_string())?
        .map(|p| source.join(&p))
        .unwrap_or(source.join("main.ar"));

    let project_files = project_files
        .into_iter()
        .filter(|path| path != &main)
        .collect::<Vec<_>>();

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

    let lib_path = format!("{}/lib/lib.ar", exe).replace('\\', "/");

    let lib = get_path(&lib_path)
        .map_err(|e| e.to_string())?
        .to_str()
        .ok_or_else(|| "Failed to convert path to string".to_string())?
        .to_string();

    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new(
        args.behavior.override_types,
    )));

    let environment = Rc::new(RefCell::new(Environment::new()));

    register_modules(project_files, type_environment.clone(), environment.clone())?;

    if let Ok(lib) = std::fs::read_to_string(lib) {
        read_input(
            lib,
            type_environment.clone(),
            environment.clone(),
            args,
            false,
        )?
    }

    let main_content = std::fs::read_to_string(main.clone())
        .map_err(|error| format!("Failed to read main file: {}", error))?;

    let result = read_input(
        main_content,
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

fn run_script(
    source: PathBuf,
    project_files: Option<Vec<PathBuf>>,
    args: &Cli,
) -> Result<(), String> {
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

    let lib_path = format!("{}/lib/lib.ar", exe).replace('\\', "/");

    let lib = get_path(&lib_path)
        .map_err(|e| e.to_string())?
        .to_str()
        .ok_or_else(|| "Failed to convert path to string".to_string())?
        .to_string();

    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new(
        args.behavior.override_types,
    )));

    let environment = Rc::new(RefCell::new(Environment::new()));

    if let Some(project_files) = project_files {
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
    args: &Cli,
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
    project_files: Vec<PathBuf>,
    type_environment: Rc<RefCell<TypeEnvironment>>,
    environment: Rc<RefCell<Environment>>,
) -> Result<(), String> {
    let source_files = project_files
        .iter()
        .map(|project_file| {
            std::fs::read_to_string(project_file)
                .map_err(|error| format!("Failed to read file: {}", error))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let token_batches = source_files
        .into_iter()
        .map(|source| shared::lexer::tokenize(&source))
        .collect::<Result<Vec<_>, _>>()?;

    let module_infos = token_batches
        .into_iter()
        .map(|tokens| parser::discover_module(tokens))
        .collect::<Result<Vec<_>, _>>()?;

    let discovery = module_infos
        .into_iter()
        .filter_map(|module_info| module_info)
        .map(|(_, module_path, module)| {
            let mod_type_environment = Rc::new(RefCell::new(TypeEnvironment::new(
                type_environment.borrow().allow_override_types,
            )));

            let discovered_types =
                discover_user_defined_types(module.clone(), mod_type_environment.clone())?;

            Ok((discovered_types, module, module_path, mod_type_environment))
        })
        .collect::<Result<Vec<_>, _>>()?;

    for (discovered_types, module, module_path, mod_type_environment) in discovery {
        let mod_environment = Rc::new(RefCell::new(Environment::new()));

        type_environment
            .borrow_mut()
            .add_module(module_path.clone(), mod_type_environment.clone());

        let value = interpreter::evaluate(typed_module, mod_environment.clone())?;

        environment
            .borrow_mut()
            .add_module(module_path, value, mod_environment.clone());
    }

    let typed_module = create_typed_ast(module, mod_type_environment.clone())?;

    Ok(())
}
