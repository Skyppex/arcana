use std::{cell::RefCell, collections::HashMap, rc::Rc};

use interpreter::Environment;
use shared::{
    ast::{self, create_ast, ModPath, Statement},
    diagnostic::{Diagnostic, SourceFile},
    pretty_print::PrettyPrint,
    type_checker::{
        add_prelude, create_typed_ast, discover_user_defined_types, simplify, TypeEnvironment,
    },
};

use crate::{cli::Cli, report::Report, spell::Spell};

pub type Rcrc<T> = Rc<RefCell<T>>;

/// Loads the core library into `type_environment` and `environment`.
///
/// Returns the type environment of `mod core` itself, which is where the
/// prelude comes from.
///
/// Core is loaded in two phases because it supplies its own prelude: the
/// modules that *are* `core` cannot have `Option` put into them, since they are
/// where `Option` is declared. Everything else in core gets it, like any other
/// module in any other spell. Glob order is not dependency order, so this
/// cannot be left to the order the files happen to be found in.
pub fn load_core(
    type_environment: Rcrc<TypeEnvironment>,
    environment: Rcrc<Environment>,
) -> Result<Rcrc<TypeEnvironment>, Report> {
    load_core_spell(
        Spell::core().map_err(Report::from)?,
        type_environment,
        environment,
    )
}

/// [`load_core`], from a directory given outright rather than looked up.
pub fn load_core_at(
    path: &std::path::Path,
    type_environment: Rcrc<TypeEnvironment>,
    environment: Rcrc<Environment>,
) -> Result<Rcrc<TypeEnvironment>, Report> {
    load_core_spell(
        Spell::core_at(path).map_err(Report::from)?,
        type_environment,
        environment,
    )
}

fn load_core_spell(
    spell: Spell,
    type_environment: Rcrc<TypeEnvironment>,
    environment: Rcrc<Environment>,
) -> Result<Rcrc<TypeEnvironment>, Report> {
    let core = ModPath::new(vec!["core".to_string()]);

    load_spell(
        &spell,
        type_environment.clone(),
        environment.clone(),
        None,
        |path| *path == core,
    )?;

    let core_type_environment = type_environment.borrow().get_module(&core).ok_or_else(|| {
        Report::from(
            Diagnostic::error("the core library declares no `mod core`").note(format!(
                "one of the .ar files under `{}` has to begin with `pub mod core;`",
                spell.root.display()
            )),
        )
    })?;

    load_spell(
        &spell,
        type_environment.clone(),
        environment,
        Some(&core_type_environment),
        |path| *path != core,
    )?;

    add_prelude(type_environment, &core_type_environment).map_err(Report::from)?;

    Ok(core_type_environment)
}

/// Type checks and evaluates every module of `spell`.
///
/// `prelude` is the core library's type environment, or `None` when the spell
/// being loaded *is* the core library. That single parameter is the whole
/// remaining difference between core and anything else.
///
/// `accept` selects which modules this pass handles, which is what lets core be
/// loaded in two phases.
pub fn load_spell(
    spell: &Spell,
    type_environment: Rcrc<TypeEnvironment>,
    environment: Rcrc<Environment>,
    prelude: Option<&Rcrc<TypeEnvironment>>,
    accept: impl Fn(&ModPath) -> bool,
) -> Result<(), Report> {
    let allow_override_types = type_environment.borrow().allow_override_types;

    // Pass 1 — read each file's module header and group the files by the module
    // they declare. The unit of compilation below is the module, not the file:
    // a module is spread over as many files as it likes.
    let mut order: Vec<ModPath> = vec![];
    let mut groups: HashMap<ModPath, Vec<(&SourceFile, Statement)>> = HashMap::new();

    for file in &spell.files {
        let report = Report::against(file);
        let tokens = shared::lexer::tokenize(&file.source).map_err(&report)?;

        let Some((_, module_path, module)) = ast::discover_module(tokens).map_err(&report)? else {
            // Skipping these silently is how a typo in a header used to delete
            // a file's contents from the build without a word, and take
            // whatever it declared out of the language with it.
            return Err(Report::from(
                Diagnostic::error(format!(
                    "`{}` does not say which module it belongs to",
                    file.name
                ))
                .note("every file in a spell begins with its module, like `pub mod core;`"),
            ));
        };

        if !accept(&module_path) {
            continue;
        }

        if !groups.contains_key(&module_path) {
            order.push(module_path.clone());
        }

        groups.entry(module_path).or_default().push((file, module));
    }

    // Pass 2 — one environment per module, and every file of the spell
    // discovered before any of it is checked, so declarations can refer to each
    // other regardless of the order the files were found in.
    let mut modules = vec![];

    for module_path in order {
        let files = groups
            .remove(&module_path)
            .expect("every path in `order` was inserted into `groups`");

        let (mod_type_environment, is_new) = type_environment
            .borrow_mut()
            .module_env(module_path.clone(), allow_override_types);

        let mod_environment = environment.borrow_mut().module_env(module_path);

        // Only into a module environment that did not exist a moment ago.
        // `add_type` rejects a name it already holds, so putting the prelude
        // into a module twice — once per file, or once per spell contributing
        // to it — reports `Option` as a duplicate declaration.
        if let (Some(core), true) = (prelude, is_new) {
            add_prelude(mod_type_environment.clone(), core).map_err(Report::from)?;
        }

        let mut discovered = vec![];

        for (file, module) in &files {
            let report = Report::against(file);

            discovered.extend(
                discover_user_defined_types(module.clone(), mod_type_environment.clone())
                    .map_err(&report)?,
            );
        }

        mod_type_environment
            .borrow_mut()
            .set_discovered_types(discovered);

        modules.push((files, mod_type_environment, mod_environment));
    }

    // Pass 3 — check and evaluate.
    for (files, mod_type_environment, mod_environment) in modules {
        for (file, module) in files {
            let report = Report::against(file);

            let typed = create_typed_ast(module, mod_type_environment.clone()).map_err(&report)?;
            let typed = simplify(typed);

            interpreter::evaluate(typed, mod_environment.clone()).map_err(&report)?;
        }
    }

    Ok(())
}

/// Compiles and runs one source file, reporting any error against it.
///
/// This is the boundary where a diagnostic stops being a value and becomes
/// text: it is the innermost place that still knows which file the spans index.
pub fn read_input(
    file: &SourceFile,
    type_environment: Rcrc<TypeEnvironment>,
    environment: Rcrc<Environment>,
    args: &Cli,
    print_result: bool,
) -> Result<(), Report> {
    compile(file, type_environment, environment, args, print_result).map_err(Report::against(file))
}

fn compile(
    file: &SourceFile,
    type_environment: Rcrc<TypeEnvironment>,
    environment: Rcrc<Environment>,
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

    let typed_program = simplify(typed_program);

    if print_simple_type_checker_ast {
        eprintln!("SIMPLIFIED:\n\n{}\n", typed_program.prettify());
    }

    let result = interpreter::evaluate(typed_program, environment)?;

    if print_tokens | print_parser_ast || print_type_checker_ast {
        eprintln!("{}", file.source);
    }

    if print_result && !result.is_none_value() {
        println!("{result}");
    }

    Ok(())
}
