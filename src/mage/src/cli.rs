use std::str::FromStr;

use clap::{ArgGroup, Parser};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
#[command(group(
    ArgGroup::new("src")
        .required(false)
        .requires("source")
        .multiple(true)
        .args(&["variables", "types"])
))]
#[command(group(
    ArgGroup::new("labeled")
        .required(false)
        .requires("src")
        .args(&["label"])
))]
pub struct Cli {
    /// Path to a folder containing a spell.toml file.
    /// If this option is not specified, mage will run interactively in your terminal.
    /// If the 'main' file isn't specified in the spell.toml file, it will look for main.ar or lib.ar in the same directory.
    /// If the spell isn't a library, it will look for a main.ar file and run it.
    /// If the source is a file with the .ar extension, it will be run as a script.
    #[arg(group = "src")]
    pub source: Option<String>,

    #[arg(short = 'r', long)]
    pub variables: bool,

    #[arg(short, long)]
    pub types: bool,

    #[arg(short, long)]
    pub label: bool,

    #[command(flatten)]
    pub logging: Logging,

    #[command(flatten)]
    pub behavior: Behavior,
}

#[derive(Parser, Clone)]
pub struct Logging {
    #[arg(short = 'v', long)]
    pub verbose: bool,

    #[arg(short = 'f', long, default_value = "")]
    pub log_flags: LogFlags,
}

#[derive(Clone)]
pub struct LogFlags {
    pub tokens: bool,
    pub ast: bool,
    pub typed_ast: bool,
}

impl FromStr for LogFlags {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut flags = LogFlags {
            tokens: false,
            ast: false,
            typed_ast: false,
        };

        for flag in s.chars() {
            match flag {
                'k' => flags.tokens = true,
                'a' => flags.ast = true,
                't' => flags.typed_ast = true,
                _ => {}
            }
        }

        Ok(flags)
    }
}

#[derive(Parser, Clone)]
pub struct Behavior {
    #[arg(short, long)]
    pub override_types: bool,
}
