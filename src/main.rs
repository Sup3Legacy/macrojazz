mod common;
#[allow(dead_code)]
mod parser;

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

    /// Source files
    sources: Vec<String>,
}

fn get_compile_mode(args: &CommandArgs) -> CompileMode {
    if args.parse_only {
        CompileMode::ParseOnly
    } else if args.type_only {
        CompileMode::TypeOnly
    } else if args.check_only {
        CompileMode::CheckOnly
    } else {
        CompileMode::Full
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
