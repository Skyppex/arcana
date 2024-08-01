use std::{
    cell::RefCell,
    fs,
    io::{self, Write},
    ops::Deref,
    path::Path,
    rc::Rc,
};

use crossterm::{
    event::{self, Event, KeyCode, KeyEventKind, KeyModifiers},
    execute,
    terminal::{self, EnterAlternateScreen, LeaveAlternateScreen},
};

use crate::{mage_args::MageArgs, read_input};
use interpreter::Environment;
use shared::type_checker::{Type, TypeEnvironment};

pub(crate) fn interactive(args: &MageArgs) -> Result<(), String> {
    let mut stdout = io::stdout();
    let _ = execute!(stdout, EnterAlternateScreen);
    let _ = terminal::enable_raw_mode();

    let mut lines = Vec::<String>::new();

    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new(
        args.behavior.override_types,
    )));
    let environment = Rc::new(RefCell::new(Environment::new()));

    'outer: loop {
        let type_environment = type_environment.clone();
        stdout.flush().unwrap();

        let mut index = 0;
        let mut input = String::new();

        loop {
            if event::poll(std::time::Duration::from_millis(500)).unwrap_or(false) {
                if let Event::Key(key_event) = event::read().map_err(|_| "Reading failed")? {
                    if key_event.kind != KeyEventKind::Press {
                        continue;
                    }

                    match key_event.code {
                        KeyCode::Enter => break,
                        KeyCode::Char(c) => {
                            // Check if the Control modifier is pressed
                            if key_event.modifiers.contains(KeyModifiers::CONTROL) {
                                match c {
                                    'c' => {
                                        break 'outer;
                                    }
                                    'w' => {
                                        // Handle Ctrl+W (e.g., delete last word)
                                        while let Some(ch) = input.pop() {
                                            write!(stdout, "\x08 \x08").unwrap(); // Move cursor back, print space to erase, move cursor back again
                                            if ch.is_whitespace() {
                                                break;
                                            }
                                        }
                                        stdout.flush().unwrap();
                                    }
                                    _ => {}
                                }
                            } else {
                                input.push(c);
                                write!(stdout, "{}", c).unwrap();
                                stdout.flush().unwrap();
                            }
                        }
                        KeyCode::Backspace => {
                            if !input.is_empty() {
                                input.pop();
                                write!(stdout, "\x08 \x08").unwrap(); // Move cursor back, print space to erase, move cursor back again
                                stdout.flush().unwrap();
                            }
                        }
                        KeyCode::Up => {
                            if index < lines.len() {
                                // Clear current input
                                write!(stdout, "\r{}", " ".repeat(input.len())).unwrap();
                                write!(stdout, "\r").unwrap();
                                stdout.flush().unwrap();

                                // Update input with the previous line
                                if index < lines.len() - 1 {
                                    index += 1;
                                }

                                input = lines[index].clone();

                                // Print the previous line
                                write!(stdout, "{}", input).unwrap();
                                stdout.flush().unwrap();
                            }
                        }
                        KeyCode::Down => {
                            // Clear current input
                            write!(stdout, "\r{}", " ".repeat(input.len())).unwrap();
                            write!(stdout, "\r").unwrap();
                            stdout.flush().unwrap();

                            index -= 1;

                            if index == 0 {
                                input = "".to_string();
                                continue;
                            }

                            // Update input with the next line
                            input = lines[index].clone();

                            // Print the next line
                            write!(stdout, "{}", input).unwrap();
                            stdout.flush().unwrap();
                        }
                        _ => {}
                    }
                }
            }
        }

        if let "q" | "quit" | "exit" = input.trim() {
            break;
        }

        lines.insert(0, input.clone());

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

        if let Err(message) = read_input(input.clone(), type_environment, environment.clone(), args)
        {
            println!("Error: {}", message);
            println!();
            println!("{}", input);
        }

        println!()
    }

    let _ = terminal::disable_raw_mode();
    let _ = execute!(stdout, LeaveAlternateScreen);

    Ok(())
}
