//! A spell is a directory of files, and a module is spread over as many of them
//! as it likes.
//!
//! The core library is one of these, found through `ARCANA_CORE_LIB_PATH`, so
//! everything here covers core as much as it covers a spell of your own.

mod common;

use std::{cell::RefCell, rc::Rc};

use common::{create_env, Fixture};
use interpreter::Environment;
use mage::{driver::load_spell, spell::Spell, Rcrc};
use shared::{ast::ModPath, type_checker::TypeEnvironment};

const ADD: &str = r#"
    pub mod math;

    pub fun add(a: Int, b: Int): Int => a + b
"#;

const SUB: &str = r#"
    pub mod math;

    pub fun sub(a: Int, b: Int): Int => a - b
"#;

/// `Debug` so a test that expected a rejection can say what loaded instead.
#[derive(Debug)]
struct Loaded {
    type_environment: Rcrc<TypeEnvironment>,
    environment: Rcrc<Environment>,
}

/// Loads a fixture as a spell, with the real core library beneath it.
fn load(fixture: &Fixture) -> Result<Loaded, String> {
    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new(false)));
    let environment = create_env();

    let core = mage::load_core(type_environment.clone(), environment.clone())
        .map_err(|error| error.to_string())?;

    let spell = Spell::read(&fixture.root, true).map_err(|error| error.to_string())?;

    load_spell(
        &spell,
        type_environment.clone(),
        environment.clone(),
        Some(&core),
        |_| true,
    )
    .map_err(|error| error.to_string())?;

    Ok(Loaded {
        type_environment,
        environment,
    })
}

#[test]
fn two_files_declaring_one_module_both_contribute() {
    // Arrange
    let fixture = Fixture::new(&[("add.ar", ADD), ("sub.ar", SUB)]);

    // Act
    let loaded = load(&fixture).expect("the spell should load");

    // Assert
    let module = loaded
        .environment
        .borrow()
        .get_module(shared::ast::ModPath::new(vec!["math".to_string()]))
        .expect("`math` should be registered");

    // Both files evaluated into one environment. Before, the second file to be
    // found replaced the first outright and whichever one lost was simply gone.
    assert!(module.borrow().get_function("add").is_some());
    assert!(module.borrow().get_function("sub").is_some());
}

#[test]
fn a_module_spread_over_subdirectories_is_still_one_module() {
    // Arrange
    // The module path comes from the header, not from where the file sits.
    let fixture = Fixture::new(&[("add.ar", ADD), ("deep/nested/sub.ar", SUB)]);

    // Act
    let loaded = load(&fixture).expect("the spell should load");

    // Assert
    let module = loaded
        .environment
        .borrow()
        .get_module(shared::ast::ModPath::new(vec!["math".to_string()]))
        .expect("`math` should be registered");

    assert!(module.borrow().get_function("add").is_some());
    assert!(module.borrow().get_function("sub").is_some());
}

#[test]
fn a_duplicate_symbol_across_two_files_of_one_module_is_rejected() {
    // Arrange
    let fixture = Fixture::new(&[("add.ar", ADD), ("again.ar", ADD)]);

    // Act
    let error = load(&fixture).expect_err("one module declaring `add` twice should be rejected");

    // Assert
    // Sharing one environment per module is what makes this reachable at all:
    // while each file had an environment to itself there was nothing to clash
    // with, and the loser just disappeared.
    assert!(
        error.contains("already exists"),
        "expected a duplicate-symbol error, got: {error}"
    );
}

#[test]
fn a_file_that_declares_no_module_is_rejected() {
    // Arrange
    let fixture = Fixture::new(&[("add.ar", ADD), ("stray.ar", "pub fun lost(): Int => 1")]);

    // Act
    let error = load(&fixture).expect_err("a file with no module header should be rejected");

    // Assert
    // These used to be skipped without a word, so a typo in a header deleted the
    // file's contents from the build and took whatever it declared with it.
    assert!(
        error.contains("stray.ar") && error.contains("does not say which module"),
        "expected the error to name the file, got: {error}"
    );
}

#[test]
fn a_spell_with_no_spell_toml_is_rejected() {
    // Arrange
    let fixture = Fixture::new(&[("add.ar", ADD)]);
    std::fs::remove_file(fixture.root.join("spell.toml")).unwrap();

    // Act
    let error = load(&fixture).expect_err("a directory without a spell.toml is not a spell");

    // Assert
    assert!(
        error.contains("spell.toml"),
        "expected the error to name spell.toml, got: {error}"
    );
}

/// Modules get a type environment each, with no parent, while the table of
/// modules lives in the root — so from inside a module there is nothing to look
/// a sibling up in.
///
/// This is a known gap rather than a decision, pinned here so that fixing it
/// breaks this test rather than going unnoticed.
#[test]
fn a_module_cannot_yet_use_a_sibling_module() {
    // Arrange
    let fixture = Fixture::new(&[
        ("add.ar", ADD),
        (
            "caller.ar",
            r#"
                pub mod caller;

                use math::{add};

                pub fun two(): Int => add(1, 1)
            "#,
        ),
    ]);

    // Act
    let result = load(&fixture);

    // Assert
    let error =
        result.expect_err("sibling imports do not work yet — if this passes, the gap is closed");

    assert!(
        error.contains("math"),
        "expected the failure to be about resolving `math`, got: {error}"
    );
}

#[test]
fn the_main_file_is_not_registered_as_a_module() {
    // Arrange
    // `main.ar` is run, not registered, so it needs no module header.
    let fixture = Fixture::new(&[("add.ar", ADD), ("main.ar", "add(1, 2)")]);

    // Act
    let loaded = load(&fixture);

    // Assert
    assert!(
        loaded.is_ok(),
        "main.ar should be left out of the module pass: {:?}",
        loaded.err()
    );
}

#[test]
fn a_spell_can_declare_which_file_is_main() {
    // Arrange
    let fixture = Fixture::new(&[
        ("spell.toml", "name = \"fixture\"\nmain = \"start.ar\"\n"),
        ("add.ar", ADD),
        ("start.ar", "add(1, 2)"),
    ]);

    // Act
    let spell = Spell::read(&fixture.root, true).expect("the spell should be read");

    // Assert
    assert!(spell.main.ends_with("start.ar"));

    // `start.ar` has no module header, so it would be rejected if it had been
    // left in the module pass.
    assert!(
        !spell
            .files
            .iter()
            .any(|file| file.name.ends_with("start.ar")),
        "the main file should not be among the module files"
    );
}

#[test]
fn both_files_of_a_module_contribute_types_too() {
    // Arrange
    let fixture = Fixture::new(&[
        ("left.ar", "pub mod shapes;\n\npub struct Dot { x: Int }"),
        (
            "right.ar",
            "pub mod shapes;\n\npub struct Line { len: Int }",
        ),
    ]);

    // Act
    let loaded = load(&fixture).expect("the spell should load");

    // Assert
    let module = loaded
        .type_environment
        .borrow()
        .get_module(shared::ast::ModPath::new(vec!["shapes".to_string()]))
        .expect("`shapes` should be registered");

    assert!(module.borrow().get_type("Dot").is_some());
    assert!(module.borrow().get_type("Line").is_some());
}

/// Two *spells* contributing to one module, which is what the core library and
/// a spell of your own both declaring `pub mod core;` would be.
///
/// Grouping the files of a single spell is not enough on its own: the second
/// `load_spell` has to find the environment the first one made rather than
/// install a new one over the top of it.
#[test]
fn a_second_spell_adds_to_a_module_rather_than_replacing_it() {
    // Arrange
    let first = Fixture::new(&[("add.ar", ADD)]);
    let second = Fixture::new(&[("sub.ar", SUB)]);

    let type_environment = Rc::new(RefCell::new(TypeEnvironment::new(false)));
    let environment = create_env();

    let core = mage::load_core(type_environment.clone(), environment.clone())
        .expect("the core library should load");

    // Act
    for fixture in [&first, &second] {
        let spell = Spell::read(&fixture.root, true).expect("the spell should be read");

        load_spell(
            &spell,
            type_environment.clone(),
            environment.clone(),
            Some(&core),
            |_| true,
        )
        .expect("the spell should load");
    }

    // Assert
    let module = environment
        .borrow()
        .get_module(ModPath::new(vec!["math".to_string()]))
        .expect("`math` should be registered");

    assert!(
        module.borrow().get_function("add").is_some(),
        "the first spell's contribution was replaced by the second"
    );

    assert!(module.borrow().get_function("sub").is_some());

    // The two module tables are separate and reuse has to hold for both: the
    // runtime one above, and the types here.
    let types = type_environment
        .borrow()
        .get_module(ModPath::new(vec!["math".to_string()]))
        .expect("`math` should be registered as a type environment too");

    assert!(
        types.borrow().get_type("add").is_some(),
        "the first spell's types were replaced by the second"
    );

    assert!(types.borrow().get_type("sub").is_some());
}
