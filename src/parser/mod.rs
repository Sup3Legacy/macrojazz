pub mod ast;

use crate::common::*;
use anyhow::Result;
pub use ast::*;
use peg::str::LineCol;

extern crate peg;

peg::parser! {
    grammar macrojazz() for str {
        // Whitespace
        rule _ = quiet!{[' ' | '\n' | '\t']* comment()*}

        rule comment() -> ()
            = "//" [^'\n']*

        pub rule program(file: SourceId) -> EarlyProgram
            = _ d:node(file)* _ ![_] { d }

        rule ident_internal(file: SourceId) -> ()
            =
                u:&quiet!{$(['a'..='z' | 'A'..='Z' | '_']
                           ['a'..='z' | 'A'..='Z' |  '_' | '0'..='9']*)}
                {?
                    match u {
                        "_" | "let" | "in" | "node" | "if" | "else" | "true" | "false" | "int" | "bool" =>
                            Err("This cannot be used as an identifier"),
                        _ => Ok(())
                    }
                }
        rule ident(file: SourceId) -> EarlyLocated<EarlyIdentifier>
            = _ start:position!()

                 (&ident_internal(file)
                  / expected!("identifier"))

                v:(quiet!{$(['a'..='z' | 'A'..='Z' | '_']
                           ['a'..='z' | 'A'..='Z' |  '_' | '0'..='9']*)}
                    / expected!("identifier"))
                end:position!() _
                {
                     EarlyLocated::new(v.to_string(), file, start, end)
                }
        rule int10_literal(file: SourceId) -> EarlyLocated<EarlyLiteral>
            = _ start:position!()
                i:(quiet!{$(['0'..='9']*)} / expected!("integer")) end:position!() _
            {?
                Ok(EarlyLocated::new(EarlyLiteral::Int(i.parse().or(Err("Int problem"))?),
                file, start, end))
            }

        rule int16_literal(file: SourceId) -> EarlyLocated<EarlyLiteral>
            = _ start:position!() "0x" i:$(['0'..='9' | 'a'..='f' | 'A'..='F']*) end:position!() _
            {?
                Ok(EarlyLocated::new(EarlyLiteral::Int(u64::from_str_radix(i, 16).or(Err("Int problem"))?),
                file, start, end))
            }

        rule int8_literal(file: SourceId) -> EarlyLocated<EarlyLiteral>
            = _ start:position!() "0o" i:$(['0'..='7']*) end:position!() _
            {?
                Ok(EarlyLocated::new(EarlyLiteral::Int(u64::from_str_radix(i, 8).or(Err("Int problem"))?),
                file, start, end))
            }

        rule int2_literal(file: SourceId) -> EarlyLocated<EarlyLiteral>
            = _ start:position!() "0b" i:$(['0'..='1']*) end:position!() _
            {?
                Ok(EarlyLocated::new(EarlyLiteral::Int(u64::from_str_radix(i, 2).or(Err("Int problem"))?),
                file, start, end))
            }

        rule int_literal(file: SourceId) -> EarlyLocated<EarlyLiteral>
            = quiet!{ int2_literal(file)
            / int8_literal(file)
            / int16_literal(file)
            / int10_literal(file) }
            / expected!("integer litteral.")

        rule bool_true_literal(file: SourceId) -> EarlyLocated<EarlyStaticLiteral>
            = _ start:position!() "true" end:position!() _
            {
                EarlyLocated::new(EarlyStaticLiteral::Bool(true), file, start, end)
            }

        rule bool_false_literal(file: SourceId) -> EarlyLocated<EarlyStaticLiteral>
            = _ start:position!() "false" end:position!() _
            {
                EarlyLocated::new(EarlyStaticLiteral::Bool(false), file, start, end)
            }

        rule bool_literal(file: SourceId) -> EarlyLocated<EarlyStaticLiteral>
            = bool_true_literal(file)
            / bool_false_literal(file)

        rule static_literal(file: SourceId) -> EarlyLocated<EarlyStaticLiteral>
            = bool_literal(file)
            / (i:int_literal(file) { i.map(|EarlyLiteral::Int(int)| EarlyStaticLiteral::Int(int) ) })

        rule literal(file: SourceId) -> EarlyLocated<EarlyLiteral>
            = int_literal(file)

        rule incl_range(file: SourceId) -> EarlyLocated<EarlyIndex>
            = _ start:position!() e1:static_expression(file)? _ "..=" e2:static_expression(file)? end:position!() _
            {
                EarlyLocated::new(
                    EarlyIndex::Range {
                        lhs: e1,
                        rhs: e2,
                        rhs_inclusive: true
                    },
                    file,
                    start,
                    end)
            }

        rule excl_range(file: SourceId) -> EarlyLocated<EarlyIndex>
            = _ start:position!() e1:static_expression(file)? _ ".." !"=" e2:static_expression(file)? end:position!() _
            {
                EarlyLocated::new(
                    EarlyIndex::Range {
                        lhs: e1,
                        rhs: e2,
                        rhs_inclusive: false
                    },
                    file,
                    start,
                    end)
            }

        rule index(file: SourceId) -> EarlyLocated<EarlyIndex>
            = incl_range(file)
            / excl_range(file)
            / (e: static_expression(file) { e.map(EarlyIndex::Simple) })

        rule static_expression(file: SourceId) -> EarlyLocated<EarlyStaticExpression> = precedence! {
                start:position!() "!" end:position!() _ x:@ {
                    let loc = x.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::MonOp {
                        operator: EarlyLocated::from_range(EarlyStaticMonOp::Not, file, start..end),
                        operand: Box::new(x),
                    }, file, loc.extend(start, end))
                }

                --

                start:position!() "-" end:position!() _ x:@ {
                    let loc = x.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::MonOp {
                        operator: EarlyLocated::from_range(EarlyStaticMonOp::Minus, file, start..end),
                        operand: Box::new(x),
                    }, file, loc.extend(start, end))
                }

                --

               x:(@) _ start:position!() "==" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyStaticBinOp::Equals, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "!=" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyStaticBinOp::NEquals, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "<" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyStaticBinOp::Lt, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() ">" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyStaticBinOp::Gt, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "<=" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyStaticBinOp::Le, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() ">=" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyStaticBinOp::Ge, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

                --

               x:(@) _ start:position!() "+" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyStaticBinOp::Plus, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "-" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyStaticBinOp::Minus, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

                 --

               x:(@) _ start:position!() "*" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyStaticBinOp::Mult, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "/" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyStaticBinOp::Div, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "%" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyStaticBinOp::Modulo, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "^" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyStaticExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyStaticBinOp::Exp, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }


                 --

                l:static_literal(file)
                    {
                        let loc = l.get_loc();
                        EarlyLocated::from_range(EarlyStaticExpression::Literal(l.get_inner()), file, loc.get_range())
                    }

                i:ident(file)
                    {
                        let loc = i.get_loc();
                        EarlyLocated::from_range(EarlyStaticExpression::Ident(i.get_inner()), file, loc.get_range())
                    }

                --

                _ start:position!() "if" _ s:static_expression(file) _ "{"
                    if_block:static_expression(file) "}" _ "else"
                    _ "{" else_block:static_expression(file) "}" end:position!() _
                    {
                        EarlyLocated::new(
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

        rule expression(file: SourceId) -> EarlyLocated<EarlyExpression> = precedence! {
                start:position!() "~" end:position!() _ x:@ {
                    let loc = x.get_loc();
                    EarlyLocated::from_range(EarlyExpression::MonOp {
                        operator: EarlyLocated::from_range(EarlyMonOp::BitNot, file, start..end),
                        operand: Box::new(x),
                    }, file, loc.extend(start, end))
                }

                 --

               x:(@) _ start:position!() "|" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyBinOp::BitOr, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

                 --


               x:(@) _ start:position!() "&" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyBinOp::BitAnd, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "^" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyBinOp::BitXOr, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

                --

               x:(@) _ start:position!() "." end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    EarlyLocated::from_range(EarlyExpression::BinOp {
                        operator: EarlyLocated::from_range(EarlyBinOp::Concat, file, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

                --

                 _ start:position!() builtin: quiet!{"@"?}
                    builtin_end:position!()
                   i:ident(file) s_args:("<" _
                   e:static_expression(file) ** "," _ ">" { e })?
                   "(" _ r_args:expression(file) ** "," _ ")"
                   end:position!() _ {
                         println!("Parsing function call.");
                         EarlyLocated::new(
                             EarlyExpression::FuncCall {
                                 func_name: i,
                                 static_params: s_args,
                                 runtime_params: r_args,
                                 builtin: EarlyLocated::new(
                                     builtin.is_some(),
                                     file,
                                     start,
                                     builtin_end)},
                                 file,
                                 start,
                                 end)
                     }

                e:@ "[" i:index(file) "]" end:position!() _
                {
                    let loc = e.get_loc();
                    EarlyLocated::from_range(
                        EarlyExpression::Index {
                            lhs: Box::new(e),
                            index: Box::new(i),
                        },
                        file,
                        loc.get_range().start..end)
                }

                 --

                l:literal(file)
                    {
                        let loc = l.get_loc();
                        EarlyLocated::from_range(EarlyExpression::Literal(l.get_inner()), file, loc.get_range())
                    }

                i:ident(file)
                    {
                        let loc = i.get_loc();
                        EarlyLocated::from_range(EarlyExpression::Ident(i.get_inner()), file, loc.get_range())
                    }

                --

                _ start:position!()
                    "if" _ s:static_expression(file) _ "{" _ if_block:expression(file) _ "}" _
                    "else" _ "{" _ else_block:expression(file) _ "}" _
                    end:position!() _
                    {
                        EarlyLocated::new(
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

                _ start:position!()
                    "let" _ r:lhs(file) _ "=" _ e:expression(file) _
                    "in" _ s:expression(file)
                    end:position!() _
                    {
                        EarlyLocated::new(
                            EarlyExpression::Let {
                                lhs: r,
                                rhs: Box::new(e),
                                scope: Box::new(s),
                            },
                            file,
                            start,
                            end
                        )
                    }

                _ start:position!() "(" _ e:expression(file) ++ "," _ ")" end:position!() _
                {
                    if e.len() == 1 {
                        e.into_iter().next().unwrap()
                    } else {
                        EarlyLocated::new(
                            EarlyExpression::Tuple(e),
                            file,
                            start,
                            end)
                    }
                }

            }

        rule wire_expression(file: SourceId) -> EarlyLocated<EarlyStaticExpression> =
            s:static_expression(file)
            {
               s
            }

        rule wire_size(file: SourceId) -> EarlyLocated<EarlyStaticExpression> =
            wire_expression(file)

        rule wire_type(file: SourceId) -> EarlyLocated<EarlyType> = precedence! {
            _ start:position!() "[" _ s:wire_size(file) _ "]" end:position!() _
            {
                EarlyLocated::new(
                    EarlyType::Base(s),
                    file,
                    start,
                    end
                )
            }

            --

            _ start:position!() "(" t:wire_type(file) ** "," ")" end:position!() _
            {
                if t.is_empty() {
                    EarlyLocated::new(
                        EarlyType::Unit,
                        file,
                        start,
                        end
                    )
                }
                else if t.len() == 1 {
                    t.into_iter().next().unwrap()
                } else {
                    EarlyLocated::new(
                        EarlyType::Tuple(t),
                        file,
                        start,
                        end
                    )
                }
            }
        }

        rule runtime_type_annot(file: SourceId) -> EarlyLocated<EarlyType>
            = _ start:position!() ":" _ r:wire_type(file) end:position!() _
            {
                r
            }

        rule runtime_arg(file: SourceId) -> EarlyLocated<EarlyArg>
            = _ start:position!() i:ident(file) typ:runtime_type_annot(file)? end:position!() _
            {
                EarlyLocated::new(EarlyArg { name: i, typ }, file, start, end)
            }

        rule runtime_arg_vec(file: SourceId) -> EarlyLocated<EarlyNodeInputType>
            = start:position!() "(" args:runtime_arg(file) ** "," ")" end:position!()
            {
                EarlyLocated::new(args, file, start, end)
            }

        rule static_type_bool(file:SourceId) -> EarlyLocated<EarlyStaticBaseType>
            = _ start:position!() _ "bool" end:position!() _
            {
                EarlyLocated::new(EarlyStaticBaseType::Bool, file, start, end)
            }

        rule static_type_int(file:SourceId) -> EarlyLocated<EarlyStaticBaseType>
            = _ start:position!() _ "int" end:position!() _
            {
                EarlyLocated::new(EarlyStaticBaseType::Int, file, start, end)
            }

        rule static_type_base(file: SourceId) -> EarlyLocated<EarlyStaticBaseType>
            = static_type_int(file)
            / static_type_bool(file)

        rule static_type_refined(file: SourceId) -> EarlyLocated<EarlyStaticType>
            = _ ":" _ start:position!() "{" base:(base:static_type_base(file) _ ":" { base })? _
            e:static_expression(file)? "}" end:position!() _
            {
                EarlyLocated::new(
                    EarlyStaticType {
                        base,
                        refinement: e,
                    },
                    file,
                    start,
                    end,
                    )
            }

        rule static_type_simple(file: SourceId) -> EarlyLocated<EarlyStaticType>
            = _ ":" _ start:position!() t:static_type_base(file) end:position!() _
            {
                EarlyLocated::new(
                    EarlyStaticType {
                        base: Some(t),
                        refinement: None,
                    },
                    file,
                    start,
                    end,
                    )
            }

        rule static_type(file: SourceId) -> Option<EarlyLocated<EarlyStaticType>>
            = s:static_type_simple(file) { Some(s) }
            / s:static_type_refined(file) { Some(s) }
            / { None }

        rule static_arg(file: SourceId) -> EarlyLocated<EarlyStaticArg>
            = _ start:position!() i:ident(file) _ t:static_type(file) end:position!() _
            {
                EarlyLocated::new(EarlyStaticArg { name: i, typ: t }, file, start, end)
            }

        rule static_arg_vec(file: SourceId) -> EarlyLocated<Vec<EarlyLocated<EarlyStaticArg>>>
            = _ start:position!() "<" _ a:static_arg(file) ** "," _ ">" end:position!()
            {
                EarlyLocated::new(a, file, start, end)
            }

        rule lhs_ident(file: SourceId) -> EarlyLocated<EarlyLhs>
            = _ start:position!() i:ident(file) end:position!() _
            {
                EarlyLocated::new(
                    EarlyLhs::Ident(i.get_inner()),
                    file,
                    start,
                    end
                )
            }

        rule lhs_tuple(file: SourceId) -> EarlyLocated<EarlyLhs>
            = _ start:position!() "(" _ i:ident(file) ** "," _ ")" end:position!() _
            {
                EarlyLocated::new(
                    EarlyLhs::Tuple(i),
                    file,
                    start,
                    end
                )
            }

        rule lhs(file: SourceId) -> EarlyLocated<EarlyLhs>
            = lhs_tuple(file)
            / lhs_ident(file)

        rule node(file: SourceId) -> EarlyLocated<EarlyNode>
            = _ start:position!() "node" _ i:ident(file) s_args:static_arg_vec(file)? r_args:runtime_arg_vec(file)
                _ "->" _ r_outs:wire_type(file) _ "{" e:expression(file) "}" end:position!() _
            {
                EarlyLocated::new(EarlyNode {
                    name: i,
                    static_args: s_args,
                    runtime_args: r_args,
                    runtime_outs: r_outs,
                    block: e,
                }, file, start, end)
            }
    }
}

pub fn parse_single(
    source_str: &str,
    source: SourceId,
) -> Result<EarlyProgram, peg::error::ParseError<LineCol>> {
    macrojazz::program(source_str, source)
}

#[cfg(test)]
mod test {
    use super::{parse_single, SourceId};

    fn run(code: &str) {
        parse_single(code, SourceId::empty()).unwrap();
    }

    #[test]
    fn io_node() {
        let code = r"
node f(a: [1], b: [n]) -> [42] {
    1
}
        ";
        run(code);
    }

    #[test]
    fn complex_idents() {
        let code = r"
node f(a9A__: [1], __b: [n]) -> [42] {
    b
}
        ";
        run(code);
    }

    #[test]
    fn let_in() {
        let code = r"
node f(a9A__: [1], __b: [n]) -> [42] {
    let (a, b) = 12 in 
        let c = a & b in 
            c . 0x12
}
        ";
        run(code);
    }

    #[test]
    #[should_panic]
    fn broken_ident() {
        let code = r"
node f(8a: [1], b: [n]) -> [42] {
    let 12_a = 1 in 1
}
        ";
        run(code);
    }

    #[test]
    #[should_panic]
    fn __as_ident() {
        let code = r"
node f(_: [2]) -> [42] {
    1
}
        ";
        run(code);
    }

    #[test]
    #[should_panic]
    fn keyword_as_identifier() {
        let code = r"
node if(a: [1], b: [n]) -> [42] {
}
        ";
        run(code);
    }

    #[test]
    fn node_call() {
        let code = r"
node f(a: [1], b: [n]) -> ((), [42]) {
    (f<n - 1, 12>(b, c[..=0]), @reg(a, b))
}
        ";
        run(code);
    }

    #[test]
    fn static_arg_node() {
        let code = r"
node f<n, m: bool>() -> () {
    1
}
        ";
        run(code);
    }

    #[test]
    fn static_runtime_combined_node() {
        let code = r"
node f<n, m: bool>(a: [n], b: [m]) -> ([n]) {
    42
}
        ";
        run(code);
    }

    #[test]
    fn runtime_ops() {
        let code = r"
node f<n>(a) -> () {
    (a . a) ^ c | d
}
        ";
        run(code);
    }

    #[test]
    fn runtime_range() {
        let code = r"
node f<n>(a) -> [_] {
    a[0..] .
    a[3..n] . 
    a[(n - 1)..=69 + n] .
    a[42]
}
        ";
        run(code);
    }

    #[test]
    fn runtime_ifthenelse() {
        let code = r"
node f<n>(a) -> [n] {
    if n == 42 { a } else { ~a }
}
        ";
        run(code);
    }

    #[test]
    fn complex_runtime() {
        let code = r"
node f<n>(a) -> [n * 2] {
    let b = (a . 0b100110)[12..=n] & if n != 0xa2 { 0b1 } else { 0b10 } in
        g<if n == 42 { 12 } else { 0o123 }>(a[..34])
}
        ";
        run(code);
    }

    #[test]
    fn refined_type() {
        let code = r"
node f<n: {int: n == 0}, m: {n != m}, o: {bool: o}>(a) -> () {
    b
}
        ";
        run(code);
    }

    #[test]
    fn multiple_nodes() {
        let code = r"
node f<n>(a) -> [_] {
    a
}

node g<m>() -> [2] {
    b
}
        ";
        run(code);
    }
}
