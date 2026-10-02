//! The core library is an ordinary spell, found through `ARCANA_CORE_LIB_PATH`.
//!
//! Everything here is about the half of that which is easy to lose: when core
//! is wrong, the error has to be about core being wrong. It used to surface as
//! `the core library does not export `Option``, several layers downstream of
//! whatever actually happened, with no span and no file.

mod common;

use std::{cell::RefCell, rc::Rc};

use common::{create_env, Fixture};
use mage::{load_core_at, spell::Spell};
use shared::type_checker::TypeEnvironment;

/// A core library good enough to compile against: the prelude and nothing else.
const OPTION: &str = r#"
    pub mod core;

    enum Option<T> {
        Some { value: T },
        None
    }
"#;

const RESULT: &str = r#"
    pub mod core;

    enum Result<TValue, TError> {
        Ok { value: TValue },
        Err { error: TError }
    }
"#;

fn load(fixture: &Fixture) -> Result<(), String> {
    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new(false)));

    load_core_at(&fixture.root, type_environment, create_env())
        .map(|_| ())
        .map_err(|error| error.to_string())
}

#[test]
fn a_core_library_of_several_files_loads() {
    // Arrange
    // No lib.ar: every file declares the module it belongs to, so a file whose
    // only content is `pub mod core;` has nothing left to do.
    let fixture = Fixture::new(&[("option.ar", OPTION), ("result.ar", RESULT)]);

    // Act
    let loaded = load(&fixture);

    // Assert
    assert!(loaded.is_ok(), "{:?}", loaded.err());
}

#[test]
fn a_broken_core_file_reports_against_itself() {
    // Arrange
    let fixture = Fixture::new(&[
        ("option.ar", OPTION),
        ("result.ar", RESULT),
        (
            "broken.ar",
            "pub mod core;\n\nfun oops(): Int => \"not an int\"",
        ),
    ]);

    // Act
    let error = load(&fixture).expect_err("a core library that does not compile should say so");

    // Assert
    // The file it happened in, the line it happened on, and a snippet — the
    // same treatment any other source gets.
    assert!(
        error.contains("broken.ar") && error.contains("not an int"),
        "expected the error to point at the core file, got:\n{error}"
    );

    assert!(
        error.contains('^'),
        "expected a rendered snippet, got:\n{error}"
    );
}

#[test]
fn a_core_library_missing_the_prelude_says_which_name_is_missing() {
    // Arrange
    let fixture = Fixture::new(&[("option.ar", OPTION)]);

    // Act
    let error = load(&fixture).expect_err("a core library without `Result` should be rejected");

    // Assert
    assert!(
        error.contains("does not export `Result`"),
        "expected the missing name, got:\n{error}"
    );

    // The note is the half that says what to do about it, and it used to be
    // dropped on the way out because the diagnostic was flattened to a string.
    assert!(
        error.contains("prelude"),
        "expected the note to survive, got:\n{error}"
    );
}

#[test]
fn a_core_library_with_no_mod_core_says_so() {
    // Arrange
    let fixture = Fixture::new(&[("elsewhere.ar", "pub mod other;\n\nfun f(): Int => 1")]);

    // Act
    let error = load(&fixture).expect_err("a core library with no `mod core` should be rejected");

    // Assert
    assert!(
        error.contains("declares no `mod core`"),
        "expected the error to name the missing module, got:\n{error}"
    );
}

#[test]
fn a_core_submodule_is_loaded_and_gets_the_prelude() {
    // Arrange
    // `core::extra` is not where `Option` is declared, so it only compiles if
    // the prelude reaches it — which is why core loads in two phases.
    let fixture = Fixture::new(&[
        ("option.ar", OPTION),
        ("result.ar", RESULT),
        (
            "extra/helpers.ar",
            r#"
                pub mod core::extra;

                pub fun first(): Option<Int> => Option::Some { value: 1 }
            "#,
        ),
    ]);

    // Act
    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new(false)));
    let environment = create_env();

    load_core_at(&fixture.root, type_environment.clone(), environment.clone())
        .unwrap_or_else(|error| panic!("the core library should load:\n{error}"));

    // Assert
    let module = environment
        .borrow()
        .get_module(shared::ast::ModPath::new(vec![
            "core".to_string(),
            "extra".to_string(),
        ]))
        .expect("`core::extra` should be registered");

    // Registered, not evaluated into a temporary and dropped — which is what
    // used to happen to everything core defined.
    assert!(module.borrow().get_function("first").is_some());
}

#[test]
fn a_directory_that_is_not_a_spell_is_rejected_as_one() {
    // Arrange
    let fixture = Fixture::new(&[("option.ar", OPTION)]);
    std::fs::remove_file(fixture.root.join("spell.toml")).unwrap();

    // Act
    let error = load(&fixture).expect_err("core has to be a spell like any other");

    // Assert
    assert!(
        error.contains("spell.toml") && error.contains("given directly"),
        "expected the error to name the manifest and where the path came from, got:\n{error}"
    );
}

#[test]
fn the_real_core_library_is_where_the_environment_says() {
    // Act
    let spell = Spell::core().expect("ARCANA_CORE_LIB_PATH should point at the core library");

    // Assert
    assert!(
        spell
            .files
            .iter()
            .any(|file| file.name.ends_with("option.ar")),
        "expected the real core library to contain option.ar, found {:?}",
        spell.files.iter().map(|f| &f.name).collect::<Vec<_>>()
    );
}
