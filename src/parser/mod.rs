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
                !"true"
                !"false"
                v:quiet!{$(['a'..='z' | 'A'..='Z']+)}
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
            / int16_literal(file)
            / int10_literal(file)

        rule bool_true_literal(file: SrcId) -> Located<EarlyStaticLiteral>
            = _ start:position!() "true" end:position!() _
            {
                Located::new(EarlyStaticLiteral::Bool(true), file, start, end)
            }

        rule bool_false_literal(file: SrcId) -> Located<EarlyStaticLiteral>
            = _ start:position!() "false" end:position!() _
            {
                Located::new(EarlyStaticLiteral::Bool(false), file, start, end)
            }

        rule bool_literal(file: SrcId) -> Located<EarlyStaticLiteral>
            = bool_true_literal(file)
            / bool_false_literal(file)

        rule static_literal(file: SrcId) -> Located<EarlyStaticLiteral>
            = bool_literal(file)
            / (i:int_literal(file) { i.map(|EarlyLiteral::Int(int)| EarlyStaticLiteral::Int(int) ) })

        rule literal(file: SrcId) -> Located<EarlyLiteral>
            = int_literal(file)

        rule incl_range(file: SrcId) -> Located<EarlyIndex>
            = _ start:position!() e1:static_expression(file)? _ "..=" e2:static_expression(file)? end:position!() _
            {
                Located::new(
                    EarlyIndex::Range {
                        lhs: e1,
                        rhs: e2,
                        rhs_inclusive: true
                    },
                    file,
                    start,
                    end)
            }

        rule excl_range(file: SrcId) -> Located<EarlyIndex>
            = _ start:position!() e1:static_expression(file)? _ ".." !"=" e2:static_expression(file)? end:position!() _
            {
                Located::new(
                    EarlyIndex::Range {
                        lhs: e1,
                        rhs: e2,
                        rhs_inclusive: false
                    },
                    file,
                    start,
                    end)
            }

        rule index(file: SrcId) -> Located<EarlyIndex>
            = incl_range(file)
            / excl_range(file)
            / (e: static_expression(file) { e.map(|exp| EarlyIndex::Simple(exp)) })

        rule static_expression(file: SrcId) -> Located<EarlyStaticExpression> = precedence! {
                start:position!() "!" end:position!() _ x:@ {
                    let loc = x.get_loc();
                    Located::empty_from_range(EarlyStaticExpression::MonOp {
                        operator: Located::empty_from_range(EarlyStaticMonOp::Not, start..end),
                        operand: Box::new(x),
                    }, loc.extend(start, end))
                }

                --

                start:position!() "-" end:position!() _ x:@ {
                    let loc = x.get_loc();
                    Located::empty_from_range(EarlyStaticExpression::MonOp {
                        operator: Located::empty_from_range(EarlyStaticMonOp::Minus, start..end),
                        operand: Box::new(x),
                    }, loc.extend(start, end))
                }

                --

               x:(@) _ start:position!() "==" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::Equals, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "!=" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::NEquals, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

                --

               x:(@) _ start:position!() "+" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::Plus, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "-" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::Minus, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

                 --
                     
               x:(@) _ start:position!() "*" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::Mult, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "/" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::Div, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "%" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::Modulo, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }


                 --

                l:static_literal(file)
                    {
                        let loc = l.get_loc();
                        Located::from_range(EarlyStaticExpression::Literal(l.get_inner()), file, loc.get_range())
                    }

                i:ident(file)
                    {
                        let loc = i.get_loc();
                        Located::from_range(EarlyStaticExpression::Ident(i.get_inner()), file, loc.get_range())
                    }

                --

                _ start:position!() "if" _ s:static_expression(file) _ "{"
                    if_block:static_expression(file) "}" _ "else" 
                    _ "{" else_block:static_expression(file) "}" end:position!() _
                    {
                        Located::new(
                            EarlyStaticExpression::IfThenElse {
                                condition: Box::new(s),
                                if_block: Box::new(if_block),
                                else_block: Box::new(else_block),
                            },
                            file,
                            start,
                            end
                            )
                    }

                --


                _ start:position!() "(" _ e:static_expression(file) _ ")" end:position!() _
                {
                    e
                }

            } 

        rule expression(file: SrcId) -> Located<EarlyExpression> = precedence! {
                start:position!() "~" end:position!() _ x:@ {
                    let loc = x.get_loc();
                    Located::empty_from_range(EarlyExpression::MonOp {
                        operator: Located::empty_from_range(EarlyMonOp::BitNot, start..end),
                        operand: Box::new(x),
                    }, loc.extend(start, end))
                }

                 --

               x:(@) _ start:position!() "|" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::BitOr, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

                 --


               x:(@) _ start:position!() "&" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::BitAnd, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "^" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::BitXOr, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

                --

               x:(@) _ start:position!() "." end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::empty_from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::Concat, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, loc_x.union(loc_y))
                }

                --

                 _ start:position!() i:ident(file) s_args:("<" _ e:static_expression(file) ** "," _ ">" { e })? "(" _ r_args:expression(file) ** "," _ ")" end:position!() _ {
                         println!("Parsing function call.");
                         Located::new(
                             EarlyExpression::FuncCall { 
                                 func_name: i, 
                                 static_params: s_args, 
                                 runtime_params: r_args },
                                 file,
                                 start,
                                 end)
                     }

                e:@ "[" i:index(file) "]" end:position!() _
                {
                    let loc = e.get_loc();
                    Located::empty_from_range(
                        EarlyExpression::Index {
                            lhs: Box::new(e),
                            index: Box::new(i),
                        },
                        loc.get_range().start..end)
                }

                 --

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

                --

                _ start:position!() 
                    "if" _ s:static_expression(file) _ "{" _ if_block:expression(file) _ "}" _
                    "else" _ "{" _ else_block:expression(file) _ "}" _
                    end:position!() _
                    {
                        Located::new(
                            EarlyExpression::IfThenElse {
                                condition: Box::new(s),
                                if_block: Box::new(if_block),
                                else_block: Box::new(else_block),
                            },
                            file,
                            start,
                            end,
                            )
                    }

                _ start:position!() "(" _ e:expression(file) ++ "," _ ")" end:position!() _
                {
                    if e.len() == 1 {
                        e.into_iter().next().unwrap()
                    } else {
                        Located::new(
                            EarlyExpression::Tuple(e),
                            file,
                            start,
                            end)
                    }
                }

            }

        rule runtime_type(file: SrcId) -> Located<EarlyStaticExpression>
            = _ start:position!() "[" exp:static_expression(file) "]" end:position!() _
            {
                exp
            }

        rule runtime_type_annot(file: SrcId) -> Located<EarlyStaticExpression>
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
            = _ start:position!() ":" _ "bool" end:position!() _
            {
                Some(Located::new(EarlyStaticType::Bool, file, start, end))
            }

        rule static_type_int(file:SrcId) -> Option<Located<EarlyStaticType>>
            = _ start:position!() ":" _ "int" end:position!() _
            {
                Some(Located::new(EarlyStaticType::Int, file, start, end))
            }

        rule static_type(file: SrcId) -> Option<Located<EarlyStaticType>>
            = static_type_bool(file)
            / static_type_int(file)
            / { None }

        rule static_arg(file: SrcId) -> Located<EarlyStaticArg>
            = _ start:position!() i:ident(file) _ t:static_type(file) end:position!() _
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

        rule statement_if_then_else(file: SrcId) -> Located<EarlyStatement>
            = _ start:position!() "if" _ s:static_expression(file) _ "{"
                if_block:block(file) "}" _ "else" _ "{" else_block:block(file) "}"
                end:position!() _
                {
                    Located::new(
                        EarlyStatement::IfThenElse {
                            condition: Box::new(s),
                            if_block,
                            else_block,
                        },
                        file,
                        start,
                        end
                        )
                }

        rule statement(file: SrcId) -> Located<EarlyStatement>
            = statement_affect(file)
            / statement_if_then_else(file)

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

#[cfg(test)]
mod test {
    use super::{parse_single, SrcId};

    fn run(code: &str) {
        parse_single(code, SrcId::empty()).unwrap();
    }

    #[test]
    fn empty_node() {
        let code = r"
node f() -> () {

}
        ";
        run(code);
    }


    #[test]
    fn io_node() {
        let code = r"
node f(a: [1], b: [n]) -> (c, d:[42]) {

}
        ";
        run(code);
    }

    #[test]
    fn static_arg_node() {
        let code = r"
node f<n, m: bool>() -> () {

}
        ";
        run(code);
    }
 
    #[test]
    fn static_runtime_combined_node() {
        let code = r"
node f<n, m: bool>(a: [n], b: [m]) -> (c: [n]) {

}
        ";
        run(code);
    }

    #[test]
    fn affect_node() {
        let code = r"
node f<n>(a) -> (b) {
    b = a;
}
        ";
        run(code);
    }

    #[test]
    fn runtime_ops() {
        let code = r"
node f<n>(a) -> (b) {
    b = (a . a) ^ c | d;
}
        ";
        run(code);
    }

    #[test]
    fn runtime_range() {
        let code = r"
node f<n>(a) -> (b) {
    b = a[0..];
    b = a[3..n];
    b = a[(n - 1)..=69 + n];
    b = a[42];
}
        ";
        run(code);
    }

    #[test]
    fn runtime_ifthenelse() {
        let code = r"
node f<n>(a) -> (b) {
    b = if n == 42 { a } else { ~a };
}
        ";
        run(code);
    }

    #[test]
    fn complex_runtime() {
        let code = r"
node f<n>(a) -> (b) {
    b = (a . 0b100110)[12..=n] & if n != 0xa2 { 0b1 } else { 0b10 };
    b = g<if n == 42 { 12 } else { 0o123 }>(a[..34]);
}
        ";
        run(code);
    }

    #[test]
    fn statement_ifthenelse() {
        let code = r"
node f<n>(a) -> (b) {
    if n == 0 {
        b = 0;
    } else {
        b = a . 0x23[..3];
    }
}
        ";
        run(code);
    }
}
