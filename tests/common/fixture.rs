use std::{
    path::{Path, PathBuf},
    sync::atomic::{AtomicUsize, Ordering},
};

/// A spell written to a temporary directory for the duration of one test.
///
/// Every test in `tests/` before this one compiled a single string. Modules
/// cannot be tested that way: the thing under test *is* the arrangement of
/// files, so there has to be an arrangement of files.
pub struct Fixture {
    pub root: PathBuf,
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);

impl Fixture {
    /// Writes `files` — `(relative path, contents)` — under a fresh directory,
    /// creating parent directories as needed. A `spell.toml` is written unless
    /// `files` provides one.
    pub fn new(files: &[(&str, &str)]) -> Fixture {
        let unique = format!(
            "arcana-test-{}-{}",
            std::process::id(),
            COUNTER.fetch_add(1, Ordering::Relaxed)
        );

        let root = std::env::temp_dir().join(unique);

        // A leftover from a crashed run would make the next one fail for a
        // reason that has nothing to do with it.
        let _ = std::fs::remove_dir_all(&root);
        std::fs::create_dir_all(&root).expect("failed to create the fixture directory");

        if !files.iter().any(|(name, _)| *name == "spell.toml") {
            write(&root, "spell.toml", "name = \"fixture\"\n");
        }

        for (name, contents) in files {
            write(&root, name, contents);
        }

        Fixture { root }
    }
}

fn write(root: &Path, name: &str, contents: &str) {
    let path = root.join(name);

    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("failed to create a fixture subdirectory");
    }

    std::fs::write(&path, contents).unwrap_or_else(|e| panic!("failed to write {name}: {e}"));
}

impl Drop for Fixture {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.root);
    }
}
