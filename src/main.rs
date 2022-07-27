mod common;
#[allow(dead_code)]
mod parser;
mod typing;

use clap::Parser;
use common::*;

#[derive(Parser, Debug)]
#[clap(author, version, about)]
struct CommandArgs {
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

fn get_compile_mode(args: &CommandArgs) -> CompileMode {
    match (args.parse_only, args.type_only, args.check_only) {
        (true, _, _) => CompileMode::ParseOnly,
        (false, true, _) => CompileMode::TypeOnly,
        (false, false, true) => CompileMode::CheckOnly,
        _ => CompileMode::Full,
    }
}

fn main() {
    let args = CommandArgs::parse();

    let compile_mode = get_compile_mode(&args);

    let mut context = CompileContext::new(compile_mode);

    for s in args.sources.into_iter() {
        context.add_from_path(s.clone()).unwrap();
    }

    context.run();
}
