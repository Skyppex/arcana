mod cli;
mod config;
mod interactive;
mod report;
mod utils;

use clap::Parser;
use config::SpellConfig;
use glob::glob;
use utils::{get_path, normalize_path};

use std::{
    cell::RefCell,
    io::{self, IsTerminal, Read},
    path::{Path, PathBuf},
    rc::Rc,
    thread,
};

use crate::cli::Cli;
use interpreter::Environment;

use report::Report;
use shared::{
    ast::{self, create_ast},
    diagnostic::{Diagnostic, SourceFile},
    pretty_print::PrettyPrint,
    type_checker::{create_typed_ast, discover_user_defined_types, TypeEnvironment},
};

const STACK_SIZE: usize = 4 * 1024 * 1024;

fn main() -> io::Result<()> {
    // Spawn thread with explicit stack size
    let child = thread::Builder::new().stack_size(STACK_SIZE).spawn(run)?;

    // Wait for thread to join
    child.join().unwrap()?;
    Ok(())
}

fn run() -> io::Result<()> {
    let args = crate::cli::Cli::parse();
    let mut stdin = io::stdin();

    let result = match (&args.source, stdin.is_terminal()) {
        (Some(source), _) => run_source(source, &args),
        (None, true) => crate::interactive::interactive(&args),
        (None, false) => {
            let mut input = String::new();
            let bytes_read = stdin.read_to_string(&mut input)?;

            if bytes_read == 0 {
                Ok(())
            } else {
                run_script("<stdin>", input, None, &args)
            }
        }
    };

    if let Err(error) = result {
        eprintln!("{error}");
        std::process::exit(1);
    }

    Ok(())
}

pub fn run_source(source: &str, args: &Cli) -> Result<(), Report> {
    let source = get_path(source).map_err(|error| Report::fatal(error.to_string()))?;

    let glob_pattern = format!("{}/**/*.ar", source.to_string_lossy()).replace('\\', "/");
    let project_files = glob(&glob_pattern).ok();

    let project_files = project_files
        .map(|paths| {
            paths
                .into_iter()
                .map(|path| path.map(normalize_path))
                .collect::<Result<Vec<_>, _>>()
                .map_err(|e| Report::fatal(e.to_string()))
        })
        .transpose()?;

    if source.is_dir() {
        let spell = source.join("spell.toml");

        if !spell.exists() {
            return Err(Report::fatal("spell.toml not found"));
        }

        let Some(project_files) = project_files else {
            return Err(Report::fatal("No files with extension .ar found in workspace"));
        };

        let spell_content = std::fs::read_to_string(spell).map_err(|error| Report::fatal(error.to_string()))?;

        let spell_config = toml::from_str::<SpellConfig>(&spell_content)
            .map_err(|e| Report::fatal(format!("Failed to parse spell.toml: {e}")))?;

        return run_spell(spell_config, project_files, &source, args);
    }

    let name = source.to_string_lossy().into_owned();

    let source = std::fs::read_to_string(source)
        .map_err(|error| Report::fatal(format!("Failed to read file: {error}")))?;

    run_script(&name, source, project_files, args)
}

fn run_spell(
    spell: SpellConfig,
    project_files: Vec<PathBuf>,
    source: &Path,
    args: &Cli,
) -> Result<(), Report> {
    let main = spell
        .main
        .map(|m| get_path(args.source.as_ref().unwrap_or(&".".to_string())).map(|p| p.join(m)))
        .transpose()
        .map_err(|error| Report::fatal(error.to_string()))?
        .map(|p| source.join(&p))
        .unwrap_or(source.join("main.ar"));

    let project_files = project_files
        .into_iter()
        .filter(|path| path != &main)
        .collect::<Vec<_>>();

    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new(
        args.behavior.override_types,
    )));

    let environment = Rc::new(RefCell::new(Environment::new()));

    let core_type_environment = load_core(type_environment.clone())?;

    register_modules(
        project_files,
        type_environment.clone(),
        environment.clone(),
        &core_type_environment,
    )?;

    let main_content = std::fs::read_to_string(main.clone())
        .map_err(|error| Report::fatal(format!("Failed to read main file: {error}")))?;

    let result = read_input(
        &SourceFile::new(main.to_string_lossy(), main_content),
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
            println!("{type_}");
        }
    }

    result
}

fn run_script(
    name: &str,
    content: String,
    project_files: Option<Vec<PathBuf>>,
    args: &Cli,
) -> Result<(), Report> {
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

    let core_type_environment = load_core(type_environment.clone())?;

    if let Some(project_files) = project_files {
        register_modules(
            project_files,
            type_environment.clone(),
            environment.clone(),
            &core_type_environment,
        )?;
    }

    let result = read_input(
        &SourceFile::new(name, content),
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
            println!("{type_}");
        }
    }

    result
}

/// The core library, compiled into the binary.
///
/// It is not looked for on disk: mage has to find it identically when run from
/// a build directory, from the nix store, or from a REPL started anywhere, and
/// every path-based scheme got one of those wrong. Editing the file rebuilds.
pub const CORE_SOURCE: &str = include_str!("../../../core/lib.ar");

/// Type checks the core library into `type_environment` and evaluates it.
///
/// Failure here is fatal rather than skipped: nothing resolves without the core
/// library, so a silent skip turns one clear error into a confusing one at
/// every use of `Option`.
pub fn load_core(
    type_environment: Rc<RefCell<TypeEnvironment>>,
) -> Result<Rc<RefCell<TypeEnvironment>>, Report> {
    let file = SourceFile::new("core/lib.ar", CORE_SOURCE);
    let report = Report::against(&file);

    let tokens = shared::lexer::tokenize(&file.source).map_err(&report)?;
    let (typed_core, core_type_environment) =
        shared::type_checker::register_core(tokens, type_environment).map_err(&report)?;

    interpreter::evaluate(typed_core, Rc::new(RefCell::new(Environment::new()))).map_err(&report)?;

    Ok(core_type_environment)
}

/// Compiles and runs one source file, reporting any error against it.
///
/// This is the boundary where a diagnostic stops being a value and becomes
/// text: it is the innermost place that still knows which file the spans index.
pub fn read_input(
    file: &SourceFile,
    type_environment: Rc<RefCell<TypeEnvironment>>,
    environment: Rc<RefCell<Environment>>,
    args: &Cli,
    print_result: bool,
) -> Result<(), Report> {
    compile(file, type_environment, environment, args, print_result)
        .map_err(Report::against(file))
}

fn compile(
    file: &SourceFile,
    type_environment: Rc<RefCell<TypeEnvironment>>,
    environment: Rc<RefCell<Environment>>,
    args: &Cli,
    print_result: bool,
) -> Result<(), Diagnostic> {
    let print_tokens = args.logging.log_flags.tokens;
    let print_parser_ast = args.logging.log_flags.ast;
    let print_type_checker_ast = args.logging.log_flags.typed_ast;
    let print_simple_type_checker_ast = args.logging.log_flags.simple_typed_ast;

    let tokens = shared::lexer::tokenize(&file.source)?;
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

    let typed_program = shared::type_checker::simplify(typed_program);

    if print_simple_type_checker_ast {
        eprintln!("SIMPLIFIED:\n\n{}\n", typed_program.prettify());
    }

    let result = interpreter::evaluate(typed_program, environment)?;

    if print_tokens | print_parser_ast || print_type_checker_ast {
        eprintln!("{}", file.source);
    }

    if print_result && !result.is_void() {
        println!("{result}");
    }

    Ok(())
}

/// Type checks and evaluates every module in the project.
///
/// Each module is compiled in an environment of its own, so each also keeps its
/// own [`SourceFile`]: a span from one module means nothing against another, and
/// an error has to be reported against the file that produced it.
pub fn register_modules(
    project_files: Vec<PathBuf>,
    type_environment: Rc<RefCell<TypeEnvironment>>,
    environment: Rc<RefCell<Environment>>,
    core_type_environment: &Rc<RefCell<TypeEnvironment>>,
) -> Result<(), Report> {
    let source_files = project_files
        .iter()
        .map(|project_file| {
            std::fs::read_to_string(project_file)
                .map(|source| SourceFile::new(project_file.to_string_lossy(), source))
                .map_err(|error| Report::fatal(format!("Failed to read file: {error}")))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let discovery = source_files
        .iter()
        .map(|file| {
            let report = Report::against(file);

            let tokens = shared::lexer::tokenize(&file.source).map_err(&report)?;
            let Some((_, module_path, module)) =
                ast::discover_module(tokens).map_err(&report)?
            else {
                return Ok(None);
            };

            let mod_type_environment = Rc::new(RefCell::new(TypeEnvironment::new(
                type_environment.borrow().allow_override_types,
            )));

            // A module is checked in an environment of its own, so the prelude
            // has to be put there too — otherwise `Option` is in scope in the
            // main file and nowhere else.
            shared::type_checker::add_prelude(mod_type_environment.clone(), core_type_environment)
                .map_err(&report)?;

            let discovered_types =
                discover_user_defined_types(module.clone(), mod_type_environment.clone())
                    .map_err(&report)?;

            Ok(Some((
                file,
                discovered_types,
                module,
                module_path,
                mod_type_environment,
            )))
        })
        .collect::<Result<Vec<_>, Report>>()?;

    for (file, discovered_types, module, module_path, mod_type_environment) in
        discovery.into_iter().flatten()
    {
        let report = Report::against(file);
        let mod_environment = Rc::new(RefCell::new(Environment::new()));

        mod_type_environment
            .borrow_mut()
            .set_discovered_types(discovered_types.clone());

        type_environment
            .borrow_mut()
            .add_module(module_path.clone(), mod_type_environment.clone());

        let typed_module =
            create_typed_ast(module, mod_type_environment.clone()).map_err(&report)?;

        let value = interpreter::evaluate(typed_module, mod_environment.clone()).map_err(&report)?;

        environment
            .borrow_mut()
            .add_module(module_path, value, mod_environment.clone());
    }

    Ok(())
}
