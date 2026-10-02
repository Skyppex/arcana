use std::{
    cell::RefCell,
    io::{self, IsTerminal, Read},
    path::Path,
    rc::Rc,
    thread,
};

use interpreter::Environment;
use shared::{diagnostic::SourceFile, type_checker::TypeEnvironment};

use mage::{
    cli::Cli,
    driver::{load_core, load_spell, read_input},
    report::Report,
    spell::Spell,
    utils::get_path,
};

use clap::Parser;

const STACK_SIZE: usize = 4 * 1024 * 1024;

fn main() -> io::Result<()> {
    // Spawn thread with explicit stack size
    let child = thread::Builder::new().stack_size(STACK_SIZE).spawn(run)?;

    // Wait for thread to join
    child.join().unwrap()?;
    Ok(())
}

fn run() -> io::Result<()> {
    let args = Cli::parse();
    let mut stdin = io::stdin();

    let result = match (&args.source, stdin.is_terminal()) {
        (Some(source), _) => run_source(source, &args),
        (None, true) => mage::interactive::interactive(&args),
        (None, false) => {
            let mut input = String::new();
            let bytes_read = stdin.read_to_string(&mut input)?;

            if bytes_read == 0 {
                Ok(())
            } else {
                run_script("<stdin>", input, &args)
            }
        }
    };

    if let Err(error) = result {
        eprintln!("{error}");
        std::process::exit(1);
    }

    Ok(())
}

fn run_source(source: &str, args: &Cli) -> Result<(), Report> {
    let source = get_path(source).map_err(|error| Report::fatal(error.to_string()))?;

    if source.is_dir() {
        return run_spell(&source, args);
    }

    let name = source.to_string_lossy().into_owned();

    let content = std::fs::read_to_string(&source)
        .map_err(|error| Report::fatal(format!("Failed to read file: {error}")))?;

    run_script(&name, content, args)
}

/// Runs the spell rooted at `root`: every module in it, then its main file.
fn run_spell(root: &Path, args: &Cli) -> Result<(), Report> {
    let spell = Spell::read(root, true).map_err(Report::from)?;

    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new(
        args.behavior.override_types,
    )));

    let environment = Rc::new(RefCell::new(Environment::new()));

    let core_type_environment = load_core(type_environment.clone(), environment.clone())?;

    load_spell(
        &spell,
        type_environment.clone(),
        environment.clone(),
        Some(&core_type_environment),
        |_| true,
    )?;

    let main_content = std::fs::read_to_string(&spell.main)
        .map_err(|error| Report::fatal(format!("Failed to read main file: {error}")))?;

    let result = read_input(
        &SourceFile::new(spell.main.to_string_lossy(), main_content),
        type_environment.clone(),
        environment.clone(),
        args,
        true,
    );

    dump(&type_environment, &environment, args);

    result
}

fn run_script(name: &str, content: String, args: &Cli) -> Result<(), Report> {
    let mut lines = content.lines();

    let content = if let Some(first_line) = lines.next() {
        if first_line.starts_with("#!") {
            lines.collect::<Vec<_>>().join("\n")
        } else {
            content
        }
    } else {
        content
    };

    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new(
        args.behavior.override_types,
    )));

    let environment = Rc::new(RefCell::new(Environment::new()));

    load_core(type_environment.clone(), environment.clone())?;

    let result = read_input(
        &SourceFile::new(name, content),
        type_environment.clone(),
        environment.clone(),
        args,
        true,
    );

    dump(&type_environment, &environment, args);

    result
}

fn dump(
    type_environment: &Rc<RefCell<TypeEnvironment>>,
    environment: &Rc<RefCell<Environment>>,
    args: &Cli,
) {
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
            println!("{type_}");
        }
    }
}
