mod common;
#[allow(dead_code)]
mod parser;
use chumsky::prelude::*;
use chumsky::Stream;

fn main() {
    //let code = "3 + 4 * > == (5 - 2939) acq node,; 0x2a 0o10 0b1111 0xlsm";
    //let tokens = parser::lexer().parse(code);
    //println!("{:?}", tokens);

    let code = "abc = f(0) > 23;";

    let len = code.chars().count();

    let (tokens, errors) = parser::lexer().parse_recovery(code);

    if let Some(tokens) = tokens {
        println!("Tokens: {:#?}", tokens);
        println!(
            "Parser: {:#?}",
            parser::statement_parser().parse(Stream::from_iter(
                len..len + 1,
                tokens.into_iter().map(|(tok, loc)| (tok, loc.get_range()))
            ))
        );
    } else {
        println!("Tokenizer: {:#?}", errors);
    }
}
