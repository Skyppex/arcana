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
pub(crate) struct MageArgs {
    /// Path to a folder containing a spell.toml file or path to the spell.toml file itself
    /// If this option is not specified, mage will run interactively in your terminal
    /// If the 'main' file isn't specified in the spell.toml file, it will look for main.ar or lib.ar in the same directory
    /// If the spell isn't a library, it will look for a main function in the main.ar file
    #[arg(short, long, group = "src")]
    pub source: Option<String>,

    #[arg(short = 'r', long)]
    pub variables: bool,

    #[arg(short, long)]
    pub types: bool,

    #[arg(short, long)]
    pub label: bool,
}