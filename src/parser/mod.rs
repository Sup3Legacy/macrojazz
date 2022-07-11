mod ast;
mod token;
use crate::common::location::{Located, Location};
use ast::*;
use chumsky::{prelude::*, stream::Stream};
use token::Token;

pub fn lexer() -> impl Parser<char, Vec<Located<Token>>, Error = Simple<char>> {
    let int = text::int(10)
        .from_str::<usize>()
        .unwrapped()
        .map(Token::Int);

    let control = one_of("()<>;,").map(Token::map_control);

    let ident = text::ident().map(Token::map_ident);

    let token = int
        .or(control)
        .or(ident)
        .recover_with(skip_then_retry_until([]));

    let comment = just("//").then(take_until(just("\n"))).padded();

    token
        .map_with_span(|tok, span| Located::from_range(tok, span))
        .padded_by(comment.repeated())
        .padded()
        .repeated()
}

pub fn expr_parser(
) -> impl Parser<Located<Token>, Located<EarlyExpression>, Error = Simple<Located<Token>>> + Clone {
    recursive(|expr| {

    })
}
