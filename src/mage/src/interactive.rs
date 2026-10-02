use std::{
    cell::RefCell,
    fs,
    io::{self, Write},
    ops::Deref,
    path::Path,
    rc::Rc,
};

use crate::{cli::Cli, driver::load_core, driver::read_input, report::Report};
use interpreter::Environment;
use shared::{
    diagnostic::SourceFile,
    type_checker::{Type, TypeEnvironment},
    types::ToKey,
};

pub fn interactive(args: &Cli) -> Result<(), Report> {
    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new(
        args.behavior.override_types,
    )));

    let environment = Rc::new(RefCell::new(Environment::new()));

    load_core(type_environment.clone(), environment.clone())?;

    loop {
        let mut input = String::new();

        io::stdout()
            .write_all(b"mage> ")
            .expect("Failed to write to stdout");

        let _ = io::stdout().flush();

        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        if let "q" | "quit" | "exit" = input.trim() {
            break;
        }

        if input.trim().starts_with("read") {
            let path = Path::new("src/mage/manual_testing");

            let file_name = input
                .split(' ')
                .skip(1)
                .collect::<Vec<&str>>()
                .join(" ")
                .replace(['\r', '\n'], "");

            let path = path.join(file_name).with_extension("ar");

            println!("Reading file: {path:?}");

            match fs::read_to_string(path) {
                Ok(source) => input = source,
                Err(e) => {
                    println!("Failed to read file: {e}");
                    continue;
                }
            }
        }

        if input.trim() == "types" {
            println!("Static members");

            for (type_, members) in type_environment.borrow().get_static_members() {
                // A name can have several candidates, one per implementation
                // that provides it.
                for (ident, candidates) in members {
                    for member_type in candidates {
                        if let Type::Function(..) = member_type {
                            println!("{type_}::{ident} -> {member_type}");
                        } else {
                            println!("{type_}::{member_type}");
                        }
                    }
                }
            }

            println!();
            println!("Types");

            for (ident, type_) in type_environment.borrow().get_types() {
                if let Type::Function(..) = type_ {
                    println!("{ident} -> {type_}");
                } else {
                    println!("{type_}");
                }
            }

            continue;
        }

        if input.trim() == "vars" {
            println!("Type environment variables:");

            for (name, type_) in type_environment.borrow().get_variables() {
                println!("{name}: {type_}");
            }

            println!();
            println!("Environment variables:");

            for (_, variable) in environment.borrow().get_variables() {
                println!("{}", variable.clone().deref().borrow().deref());
            }

            continue;
        }

        if input.trim() == "varkeys" {
            println!("Type environment variables:");

            for (name, type_) in type_environment.borrow().get_variables() {
                println!("{}: {}", name, type_.to_key());
            }

            println!();
            println!("Environment variables:");

            for (_, variable) in environment.borrow().get_variables() {
                println!("{}", variable.clone().deref().borrow().deref());
            }

            continue;
        }

        if input.trim() == "varsd" {
            for (name, variable) in type_environment.borrow().get_variables() {
                println!("{name}: {variable:?}");
            }
            continue;
        }

        if input.trim() == "env" {
            for (_, value) in environment.borrow().get_variables() {
                println!("{}", value.borrow());
            }

            for (_, value) in environment.borrow().get_functions() {
                println!("{}", value.borrow());
            }

            continue;
        }

        // The report already quotes the offending line, so the input is not
        // echoed back after it.
        if let Err(report) = read_input(
            &SourceFile::new("<repl>", input.clone()),
            type_environment.clone(),
            environment.clone(),
            args,
            true,
        ) {
            println!("{report}");
        }

        println!()
    }

    Ok(())
}
