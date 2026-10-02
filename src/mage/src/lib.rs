//! The `mage` driver.
//!
//! This is a library so that the binary and the test suite load the core
//! library through the same code. They used to have a strategy each — an
//! `include_str!` in the binary and a `CARGO_MANIFEST_DIR` path in the tests —
//! which meant a change to how core is laid out broke them independently.

pub mod cli;
pub mod config;
pub mod driver;
pub mod interactive;
pub mod report;
pub mod spell;
pub mod utils;

pub use driver::{load_core, load_core_at, load_spell, read_input, Rcrc};
pub use report::Report;
pub use spell::{Spell, CORE_PATH_VAR};
