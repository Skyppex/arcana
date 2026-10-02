use std::path::{Path, PathBuf};

use glob::glob;
use shared::diagnostic::{Diagnostic, SourceFile};

use crate::{config::SpellConfig, utils::normalize_path};

/// The environment variable naming the core library's directory.
pub const CORE_PATH_VAR: &str = "ARCANA_CORE_LIB_PATH";

/// The core library path as it stood when `mage` was compiled.
///
/// `.cargo/config.toml` sets `ARCANA_CORE_LIB_PATH` for the build as well as
/// for `cargo run` and `cargo test`, so a binary built from a checkout knows
/// where that checkout's core library is. That is what lets
/// `./target/debug/mage` work from any directory without a wrapper. The
/// variable read at runtime always wins, so the nix wrapper overrides the path
/// baked into the sandbox build.
const BAKED_CORE_PATH: Option<&str> = option_env!("ARCANA_CORE_LIB_PATH");

/// Where a core library path came from, so an error can say.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Origin {
    Environment,
    Baked,
    Given,
}

impl Origin {
    fn note(self, path: &Path) -> String {
        match self {
            Origin::Environment => {
                format!("{CORE_PATH_VAR} is set to `{}`", path.display())
            }
            Origin::Baked => format!(
                "`{}` is the path recorded when mage was built; set {CORE_PATH_VAR} to override it",
                path.display()
            ),
            Origin::Given => format!("`{}` was given directly", path.display()),
        }
    }
}

/// A spell: a `spell.toml` and every `.ar` file beneath it.
///
/// The core library is one of these, found through [`Spell::core`] rather than
/// compiled in. Everything downstream treats it like any other — which is the
/// point, since core used to be the one spell whose failures surfaced as
/// something other than what went wrong.
pub struct Spell {
    pub root: PathBuf,
    pub config: SpellConfig,
    /// The file this spell is run from, whether or not it exists. A library
    /// spell — the core library is one — simply never has it asked for.
    pub main: PathBuf,
    pub files: Vec<SourceFile>,
}

impl Spell {
    /// Reads the spell rooted at `root`: its `spell.toml`, then every `.ar`
    /// file under it, recursively.
    ///
    /// `skip_main` leaves out the spell's main file, which is run rather than
    /// registered as a module. A library spell is read without it.
    pub fn read(root: &Path, skip_main: bool) -> Result<Spell, Diagnostic> {
        let manifest = root.join("spell.toml");

        let config = std::fs::read_to_string(&manifest)
            .map_err(|error| {
                Diagnostic::error(format!("could not read `{}`: {error}", manifest.display()))
                    .note("a spell is a directory with a spell.toml in it")
            })
            .and_then(|content| {
                toml::from_str::<SpellConfig>(&content).map_err(|error| {
                    Diagnostic::error(format!("could not parse `{}`: {error}", manifest.display()))
                })
            })?;

        let pattern = format!("{}/**/*.ar", root.to_string_lossy()).replace('\\', "/");

        // A bad pattern used to become "no files found", which describes the
        // wrong problem entirely.
        let paths = glob(&pattern)
            .map_err(|error| {
                Diagnostic::error(format!("could not search `{}`: {error}", root.display()))
            })?
            .map(|path| {
                path.map(normalize_path).map_err(|error| {
                    Diagnostic::error(format!(
                        "could not read a file under `{}`: {error}",
                        root.display()
                    ))
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let main =
            normalize_path(root.join(config.main.clone().unwrap_or_else(|| "main.ar".to_string())));

        let skip = skip_main.then(|| main.clone());

        let files = paths
            .into_iter()
            .filter(|path| Some(path) != skip.as_ref())
            .map(|path| {
                let source = std::fs::read_to_string(&path).map_err(|error| {
                    Diagnostic::error(format!("could not read `{}`: {error}", path.display()))
                })?;

                Ok(SourceFile::new(display_name(root, &path), source))
            })
            .collect::<Result<Vec<_>, Diagnostic>>()?;

        Ok(Spell {
            root: root.to_path_buf(),
            config,
            main,
            files,
        })
    }

    /// The core library, located through [`CORE_PATH_VAR`].
    ///
    /// Each way of failing to find it gets its own message. The one that
    /// matters is the last: a path that is a real directory but not the core
    /// library used to report as "the core library does not export `Option`",
    /// several layers downstream of the actual mistake.
    pub fn core() -> Result<Spell, Diagnostic> {
        let env = std::env::var(CORE_PATH_VAR).ok();
        let (path, origin) = resolve(env.as_deref(), BAKED_CORE_PATH)?;

        Spell::core_from(&path, origin)
    }

    /// The core library at a path given outright, bypassing the environment.
    ///
    /// Tests use this so that checking what happens with a broken core library
    /// does not mean mutating the environment of every other test running
    /// beside it.
    pub fn core_at(path: &Path) -> Result<Spell, Diagnostic> {
        Spell::core_from(path, Origin::Given)
    }

    fn core_from(path: &Path, origin: Origin) -> Result<Spell, Diagnostic> {
        if !path.is_dir() {
            return Err(Diagnostic::error(format!(
                "the core library is not at `{}`",
                path.display()
            ))
            .note(origin.note(path))
            .note("it should be a directory containing the core library's spell.toml"));
        }

        let spell = Spell::read(path, false).map_err(|error| error.note(origin.note(path)))?;

        if spell.files.is_empty() {
            return Err(Diagnostic::error(format!(
                "the core library at `{}` has no .ar files in it",
                path.display()
            ))
            .note(origin.note(path)));
        }

        Ok(spell)
    }
}

/// Picks the core library path, preferring the running environment over the
/// path recorded at build time.
///
/// Split out from reading the environment so it can be tested without a test
/// mutating process-wide state that every other test shares.
fn resolve(env: Option<&str>, baked: Option<&str>) -> Result<(PathBuf, Origin), Diagnostic> {
    if let Some(path) = env.filter(|p| !p.trim().is_empty()) {
        return Ok((PathBuf::from(path), Origin::Environment));
    }

    if let Some(path) = baked.filter(|p| !p.trim().is_empty()) {
        return Ok((PathBuf::from(path), Origin::Baked));
    }

    Err(
        Diagnostic::error("mage cannot find the core library").note(format!(
            "set {CORE_PATH_VAR} to the directory the core library lives in"
        )),
    )
}

/// `core/option.ar` rather than `/nix/store/1a2b…/share/arcana/core/option.ar`.
///
/// Diagnostics name the file they point at, and the absolute path is both the
/// widest line in the error and the least informative part of it.
fn display_name(root: &Path, path: &Path) -> String {
    let relative = path.strip_prefix(root).unwrap_or(path);

    match root.file_name() {
        Some(name) => Path::new(name)
            .join(relative)
            .to_string_lossy()
            .into_owned(),
        None => relative.to_string_lossy().into_owned(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_environment_is_preferred_over_the_baked_in_path() {
        let (path, origin) = resolve(Some("/from/env"), Some("/from/build")).unwrap();

        assert_eq!(path, PathBuf::from("/from/env"));
        assert_eq!(origin, Origin::Environment);
    }

    #[test]
    fn the_baked_in_path_is_used_when_the_environment_is_unset() {
        let (path, origin) = resolve(None, Some("/from/build")).unwrap();

        assert_eq!(path, PathBuf::from("/from/build"));
        assert_eq!(origin, Origin::Baked);
    }

    /// An exported variable set to nothing is a likelier mistake than a
    /// deliberate choice, so it falls through rather than naming the empty path.
    #[test]
    fn an_empty_environment_variable_counts_as_unset() {
        let (path, origin) = resolve(Some("   "), Some("/from/build")).unwrap();

        assert_eq!(path, PathBuf::from("/from/build"));
        assert_eq!(origin, Origin::Baked);
    }

    #[test]
    fn with_neither_the_error_says_which_variable_to_set() {
        let error = resolve(None, None).unwrap_err();

        assert_eq!(error.to_string(), "mage cannot find the core library");
        assert!(error.notes.iter().any(|note| note.contains(CORE_PATH_VAR)));
    }

    #[test]
    fn each_origin_explains_itself_differently() {
        let path = Path::new("/somewhere/core");

        assert!(Origin::Environment.note(path).contains(CORE_PATH_VAR));
        assert!(Origin::Baked.note(path).contains("when mage was built"));
        assert!(Origin::Given.note(path).contains("given directly"));
    }
}
