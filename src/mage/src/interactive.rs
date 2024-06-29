use std::{
    cell::RefCell,
    fs,
    io::{stdin, stdout, Write},
    path::Path,
    rc::Rc,
};

use crate::read_input;
use interpreter::Environment;
use shared::type_checker::{Type, TypeEnvironment};

pub(crate) fn interactive() -> Result<(), String> {
    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new()));
    let environment = Rc::new(RefCell::new(Environment::new()));

    loop {
        let type_environment = type_environment.clone();
        let _ = stdout().flush();
        let mut input = String::new();
        stdin().read_line(&mut input).expect("Failed to read line");

        if let "q" | "quit" | "exit" = input.trim() {
            break Ok(());
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
            for (name, variable) in type_environment.borrow().get_variables() {
                println!("{}: {}", name, variable);
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

        if let Err(message) = read_input(input.clone(), type_environment, environment.clone()) {
            println!("Error: {}", message);
            println!();
            println!("{}", input);
        }

        println!()
    }
}
