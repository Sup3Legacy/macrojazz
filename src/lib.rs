use std::slice::Iter;

pub mod builtin;
pub mod common;
pub mod parser;
pub mod pretyping;
pub mod typing;

use clap::Parser;
use common::*;

#[derive(Parser, Debug)]
#[clap(author, version, about)]
pub struct CommandArgs {
    /// Stop after parsing
    #[clap(long, value_parser)]
    parse_only: bool,

    /// Stop after typing
    #[clap(long, value_parser)]
    type_only: bool,

    /// Stop after checking
    #[clap(long, value_parser)]
    check_only: bool,

    /// Ignore dead code
    #[clap(long, value_parser)]
    ignore_dead_code: bool,

    /// Ignore unused arguments
    #[clap(long, value_parser)]
    ignore_unused_args: bool,

    /// Print some debug information along the way
    #[clap(short, long, value_parser)]
    verbose: bool,

    /// Source files
    sources: Vec<String>,

    /// Top-level node
    #[clap(short = 'n', long = "node")]
    top_node: Option<String>,

    /// Output file path
    #[clap(short = 'o', long = "out")]
    out_path: Option<String>,
}

impl CommandArgs {
    pub fn get_compile_mode(&self) -> CompileMode {
        match (self.parse_only, self.type_only, self.check_only) {
            (true, _, _) => CompileMode::ParseOnly,
            (false, true, _) => CompileMode::TypeOnly,
            (false, false, true) => CompileMode::CheckOnly,
            _ => CompileMode::Full,
        }
    }

    pub fn iter_sources(&'_ self) -> Iter<'_, String> {
        self.sources.iter()
    }

    pub fn sources_number(&self) -> usize {
        self.sources.len()
    }
}
