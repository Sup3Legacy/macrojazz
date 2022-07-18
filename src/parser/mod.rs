mod ast;

use crate::common::location::*;
use anyhow::{anyhow, Context, Result};
use ariadne::{FileCache, Source};
pub use ast::*;
use peg::str::LineCol;
use std::{path::PathBuf, sync::Arc};

extern crate peg;

peg::parser! {
    grammar macrojazz() for str {
        // Whitespace
        rule _ = quiet!{[' ' | '\n' | '\t']* comment()*}

        rule comment() -> ()
            = "//" [^'\n']*

        pub rule program(file: SrcId) -> EarlyProgram
            = _ d:node(file)* _ ![_] { d }

        rule ident(file: SrcId) -> Located<EarlyIdentifier>
            = _ start:position!()
                !"node"
                !"if"
                !"else"
                !"end"
                !"true"
                !"false"
                v:quiet!{$(['a'..='z' | 'A'..='Z']*)}
                end:position!() _
                {
                    Located::new(v.to_string(), file, start, end)
                }
        rule int10_literal(file: SrcId) -> Located<EarlyLiteral>
            = _ start:position!() i:$(['0'..='9']*) end:position!() _
            {?
                Ok(Located::new(EarlyLiteral::Int(i.parse().or(Err("Int problem"))?),
                file, start, end))
            }

        rule int16_literal(file: SrcId) -> Located<EarlyLiteral>
            = _ start:position!() "0x" i:$(['0'..='9' | 'a'..='f' | 'A'..='F']*) end:position!() _
            {?
                Ok(Located::new(EarlyLiteral::Int(usize::from_str_radix(i, 16).or(Err("Int problem"))?),
                file, start, end))
            }

        rule int8_literal(file: SrcId) -> Located<EarlyLiteral>
            = _ start:position!() "0o" i:$(['0'..='7']*) end:position!() _
            {?
                Ok(Located::new(EarlyLiteral::Int(usize::from_str_radix(i, 8).or(Err("Int problem"))?),
                file, start, end))
            }

        rule int2_literal(file: SrcId) -> Located<EarlyLiteral>
            = _ start:position!() "0b" i:$(['0'..='1']*) end:position!() _
            {?
                Ok(Located::new(EarlyLiteral::Int(usize::from_str_radix(i, 2).or(Err("Int problem"))?),
                file, start, end))
            }

        rule int_literal(file: SrcId) -> Located<EarlyLiteral>
            = int2_literal(file)
            / int8_literal(file)
            / int10_literal(file)
            / int16_literal(file)

        rule bool_true_literal(file: SrcId) -> Located<EarlyLiteral>
            = _ start:position!() "true" end:position!() _
            {
                Located::new(EarlyLiteral::Bool(true), file, start, end)
            }

        rule bool_false_literal(file: SrcId) -> Located<EarlyLiteral>
            = _ start:position!() "false" end:position!() _
            {
                Located::new(EarlyLiteral::Bool(false), file, start, end)
            }

        rule bool_literal(file: SrcId) -> Located<EarlyLiteral>
            = bool_true_literal(file)
            / bool_false_literal(file)

        rule literal(file: SrcId) -> Located<EarlyLiteral>
            = bool_literal(file)
            / int_literal(file)

        rule expression(file: SrcId) -> Located<EarlyExpression> = precedence! {
                start:position!() "!" end:position!() _ x:@ {
                    let loc = x.get_loc();
                    Located::empty_from_range(EarlyExpression::MonOp {
                        operator: Located::empty_from_range(EarlyMonOp::Not, start..end),
                        operand: Box::new(x),
                    }, loc.extend(start, end))
                }

                --

                start:position!() "~" end:position!() _ x:@ {
                    let loc = x.get_loc();
                    Located::empty_from_range(EarlyExpression::MonOp {
                        operator: Located::empty_from_range(EarlyMonOp::BitNot, start..end),
                        operand: Box::new(x),
                    }, loc.extend(start, end))
                }

                --

                start:position!() "-" end:position!() _ x:@ {
                    let loc = x.get_loc();
                    Located::empty_from_range(EarlyExpression::MonOp {
                        operator: Located::empty_from_range(EarlyMonOp::Minus, start..end),
                        operand: Box::new(x),
                    }, loc.extend(start, end))
                }

                --

               x:(@) _ start:position!() "==" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::Equals, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "!=" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::NEquals, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

                --

               x:(@) _ start:position!() ">" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::Greater, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() ">=" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::GreaterEq, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "<" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::Smaller, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "<=" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::SmallerEq, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

                l:literal(file)
                    {
                        let loc = l.get_loc();
                        Located::from_range(EarlyExpression::Literal(l.get_inner()), file, loc.get_range())
                    }

                i:ident(file)
                    {
                        let loc = i.get_loc();
                        Located::from_range(EarlyExpression::Ident(i.get_inner()), file, loc.get_range())
                    }

            }

        rule runtime_type(file: SrcId) -> Located<EarlyExpression>
            = _ start:position!() "[" exp:expression(file) "]" end:position!() _
            {
                exp
            }

        rule runtime_type_annot(file: SrcId) -> Located<EarlyExpression>
            = _ start:position!() ":" _ r:runtime_type(file) end:position!() _
            {
                r
            }

        rule runtime_arg(file: SrcId) -> Located<EarlyRuntimeArg>
            = _ start:position!() i:ident(file) typ:runtime_type_annot(file)? end:position!() _
            {
                Located::new(EarlyRuntimeArg { name: i, typ }, file, start, end)
            }

        rule runtime_arg_vec(file: SrcId) -> Located<Vec<Located<EarlyRuntimeArg>>>
            = start:position!() "(" args:runtime_arg(file) ** "," ")" end:position!()
            {
                Located::new(args, file, start, end)
            }

        rule static_type_bool(file:SrcId) -> Option<Located<EarlyStaticType>>
            = _ start:position!() "bool" end:position!() _
            {
                Some(Located::new(EarlyStaticType::Bool, file, start, end))
            }

        rule static_type_int(file:SrcId) -> Option<Located<EarlyStaticType>>
            = _ start:position!() "int" end:position!() _
            {
                Some(Located::new(EarlyStaticType::Int, file, start, end))
            }

        rule static_type(file: SrcId) -> Option<Located<EarlyStaticType>>
            = static_type_bool(file)
            / static_type_int(file)
            / { None }

        rule static_arg(file: SrcId) -> Located<EarlyStaticArg>
            = _ start:position!() i:ident(file) _ ":" _ t:static_type(file) end:position!() _
            {
                Located::new(EarlyStaticArg { name: i, typ: t }, file, start, end)
            }

        rule static_arg_vec(file: SrcId) -> Located<Vec<Located<EarlyStaticArg>>>
            = _ start:position!() "<" _ a:static_arg(file) ** "," _ ">" end:position!()
            {
                Located::new(a, file, start, end)
            }

        rule lhs_ident(file: SrcId) -> EarlyStatementLhs
            = i:ident(file)
            {
                EarlyStatementLhs::Ident(i)
            }

        rule lhs_tuple(file: SrcId) -> EarlyStatementLhs
            = _ start:position!() "(" _ i:ident(file) ** "," _ ")" end:position!() _
            {
                EarlyStatementLhs::Tuple(Located::new(i, file, start, end))
            }

        rule lhs(file: SrcId) -> EarlyStatementLhs
            = lhs_tuple(file)
            / lhs_ident(file)

        rule statement_affect(file: SrcId) -> Located<EarlyStatement>
            = _ start:position!() l:lhs(file) "=" e:expression(file) ";" end:position!() _
            {
                Located::new(EarlyStatement::Affect { lhs: l, rhs: Box::new(e) }, file, start, end)
            }

        rule statement(file: SrcId) -> Located<EarlyStatement>
            = statement_affect(file)

        rule block(file: SrcId) -> Located<EarlyBlock>
            = _ start:position!() b:statement(file)* end:position!() _
            {
                Located::new(b, file, start, end)
            }

        rule node(file: SrcId) -> Located<EarlyNode>
            = _ start:position!() "node" _ i:ident(file) s_args:static_arg_vec(file)? r_args:runtime_arg_vec(file)
                _ "->" _ r_outs:runtime_arg_vec(file) _ "{" b:block(file) "}" end:position!() _
            {
                Located::new(EarlyNode {
                    name: i,
                    static_args: s_args,
                    runtime_args: r_args,
                    runtime_outs: r_outs,
                    block: b,
                }, file, start, end)
            }
    }
}

pub fn parse_single(
    source_str: &str,
    src: SrcId,
) -> Result<EarlyProgram, peg::error::ParseError<LineCol>> {
    macrojazz::program(source_str, src)
}

fn parse_single_source(source: &str) -> Result<EarlyProgram> {
    todo!()
}

pub fn parse_sources(sources: Vec<PathBuf>) -> Result<(EarlyProgram, FileCache)> {
    let mut filecache = FileCache::default();

    for src in sources {}

    todo!()
}
