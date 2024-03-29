use std::{cell::RefCell, fs, io::{stdin, stdout, Write}, path::Path, rc::Rc};

use shared::type_checker::TypeEnvironment;

use crate::{interpreter::environment::Environment, read_input};


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

        if input.trim() == "types" {
            for (.., type_) in type_environment.borrow().get_types() {
                println!("{}", type_);
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

        if let Err(message) = read_input(input, type_environment, environment.clone()) {
            println!("Error: {}", message);
        }

        println!()
    }
}
