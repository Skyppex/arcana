use std::{
    cell::RefCell,
    fs,
    io::{self, Write},
    ops::Deref,
    path::Path,
    rc::Rc,
};

use crate::{mage_args::MageArgs, read_input, utils::get_path};
use interpreter::Environment;
use shared::type_checker::{Type, TypeEnvironment};

pub(crate) fn interactive(args: &MageArgs) -> Result<(), String> {
    let lib = get_path("lib/lib.ar")
        .map_err(|e| e.to_string())?
        .to_str()
        .ok_or_else(|| "Failed to convert path to string".to_string())?
        .to_string();

    let lib =
        std::fs::read_to_string(lib).map_err(|error| format!("Failed to read file: {}", error))?;

    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new(
        args.behavior.override_types,
    )));

    let environment = Rc::new(RefCell::new(Environment::new()));

    read_input(lib, type_environment.clone(), environment.clone(), args)?;

    loop {
        let _ = io::stdout().flush();
        let mut input = String::new();
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

            println!("Reading file: {:?}", path);

            match fs::read_to_string(path) {
                Ok(source) => input = source,
                Err(e) => {
                    println!("Failed to read file: {}", e);
                    continue;
                }
            }
        }

        if input.trim() == "types" {
            for (ident, type_) in type_environment.borrow().get_types() {
                if let Type::Function(..) = type_ {
                    println!("{} -> {}", ident.to_string(), type_);
                } else {
                    println!("{}", type_);
                }
            }
            continue;
        }

        if input.trim() == "vars" {
            println!("Type environment variables:");
            for (name, type_) in type_environment.borrow().get_variables() {
                println!("{}: {}", name, type_);
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
                println!("{}: {:?}", name, variable);
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

        if let Err(message) = read_input(
            input.clone(),
            type_environment.clone(),
            environment.clone(),
            args,
        ) {
            println!("Error: {}", message);
            println!();
            println!("{}", input);
        }

        println!()
    }

    Ok(())
}
