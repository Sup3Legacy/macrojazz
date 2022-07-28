use macrojazz::*;

use clap::Parser;
use common::*;


fn main() {
    let args = CommandArgs::parse();

    let compile_mode = args.get_compile_mode();

    let mut context = CompileContext::new(compile_mode);

    if args.sources_number() == 0 {
        
    }

    for s in args.iter_sources() {
        context.add_from_path(s.clone()).unwrap();
    }

    context.run();
}
