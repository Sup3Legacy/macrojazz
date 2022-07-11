mod ast;
mod token;
use crate::common::location::{Located, Location};
use ast::*;
use chumsky::{prelude::*, stream::Stream};
use token::Token;

pub fn lexer() -> impl Parser<char, Vec<(Token, Location)>, Error = Simple<char>> {
    let int = text::int(10)
        .from_str::<usize>()
        .unwrapped()
        .map(Token::Int);

    let control = one_of("()<>;,").map(Token::map_control);

    let op = one_of("+-*/!=<>")
        .repeated()
        .at_least(1)
        .collect::<String>()
        .try_map(|s, span| {
            Token::match_op(&s).map_err(|_| Simple::custom(span, format!("Unknown operator {}", s)))
        });

    let ident = text::ident().map(Token::map_ident);

    let token = int
        .or(control)
        .or(op)
        .or(ident)
        .map_with_span(move |tok, span| (tok, Location::empty_from_range(span)))
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just("\n"))).padded();

    token.padded_by(comment.repeated()).padded().repeated()
}

pub fn expr_parser(
) -> impl Parser<Token, Located<EarlyExpression>, Error = Simple<Located<Token>>> + Clone {
    todo!()
}

mod tests {
    use super::*;
    use chumsky::Parser;

    #[test]
    fn simple() {
        let code = r#"4 + 5*2"#;

        let tokens = lexer().parse(code).unwrap();

        println!("{:?}", tokens);
    }
}
