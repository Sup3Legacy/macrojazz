mod common;
#[allow(dead_code)]
mod parser;
use chumsky::prelude::*;
use chumsky::Stream;

fn main() {
    //let code = "3 + 4 * > == (5 - 2939) acq node,; 0x2a 0o10 0b1111 0xlsm";
    //let tokens = parser::lexer().parse(code);
    //println!("{:?}", tokens);

    let code = "function()";

    let len = code.chars().count();

    if let Ok(tokens) = parser::lexer().parse(code) {
        println!(
            "{:?}",
            parser::expr_parser().parse(Stream::from_iter(
                len..len + 1,
                tokens.into_iter().map(|(tok, loc)| (tok, loc.get_range()))
            ))
        );
    }
}
