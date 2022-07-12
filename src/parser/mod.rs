mod ast;
mod token;
use crate::common::location::{Located, Location};
use ast::*;

use chumsky::prelude::*;
use token::Token;

pub fn lexer() -> impl Parser<char, Vec<(Token, Location)>, Error = Simple<char>> {
    let int = (just("0").ignore_then(
        (just("x")
            .ignore_then(text::int(16))
            .map(|s: String| usize::from_str_radix(s.as_str(), 16).unwrap()))
        .or(just("o")
            .ignore_then(text::int(8))
            .map(|s: String| usize::from_str_radix(s.as_str(), 8).unwrap()))
        .or(just("b")
            .ignore_then(text::int(2))
            .map(|s: String| usize::from_str_radix(s.as_str(), 2).unwrap())),
    ))
    .or(text::int(10).from_str::<usize>().unwrapped())
    .map(Token::Int);

    let control = one_of("();,").map(Token::map_control);

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

pub fn expr_parser() -> impl Parser<Token, Located<EarlyExpression>, Error = Simple<Token>> + Clone
{
    recursive(|expr| {
        let ident = select! { Token::Ident(id) => id }
            .labelled("identifier")
            .map_with_span(|id, span: std::ops::Range<usize>| {
                Located::empty_from_range(EarlyExpression::Ident(id), span)
            });

        let immediate = select! {
            Token::Bool(b) => EarlyImmediate::Bool(b),
            Token::Int(i) => EarlyImmediate::Int(i),
        }
        .labelled("immediate value")
        .map_with_span(|imm, span: std::ops::Range<usize>| {
            Located::empty_from_range(EarlyExpression::Immediate(imm), span)
        });

        let atoms = ident.or(immediate).or(expr
            .clone()
            .delimited_by(just(Token::ParL), just(Token::ParR)));

        let range = expr.clone().then(just(Token::DotDot)).then(expr.clone());

        let items = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing();

        let call = select! { Token::Ident(id) => id }
            .map_with_span(Located::empty_from_range)
            .then(
                items
                    .clone()
                    .delimited_by(just(Token::Lt), just(Token::Gt))
                    .map_with_span(|vec, span| (vec, span))
                    .or_not(),
            )
            .then(
                items
                    .clone()
                    .delimited_by(just(Token::ParL), just(Token::ParR))
                    .map_with_span(|vec, span| (vec, span)),
            )
            .map_with_span(
                |((id, static_args), runtime_args), span: std::ops::Range<usize>| {
                    Located::empty_from_range(
                        EarlyExpression::FuncCall {
                            func_name: id,
                            static_params: static_args
                                .map(|(sp, span)| Located::empty_from_range(sp, span)),
                            runtime_params: Located::empty_from_range(
                                runtime_args.0,
                                runtime_args.1,
                            ),
                        },
                        span,
                    )
                },
            );

        call.or(ident).or(immediate)
    })
}

mod tests {
    use super::*;

    #[test]
    fn simple() {
        let code = r#"4 + 5*2"#;

        let tokens = lexer().parse(code).unwrap();

        println!("{:?}", tokens);
    }
}
