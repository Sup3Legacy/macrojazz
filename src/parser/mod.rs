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

        rule ident(file: SourceId) -> Located<EarlyIdentifier>
            = _ start:position!()
                !"node"
                !"if"
                !"else"
                !"true"
                !"false"
                v:quiet!{$(['a'..='z' | 'A'..='Z' | '_']
                           ['a'..='z' | 'A'..='Z' |  '_' | '0'..='9']*)}
                end:position!() _
                {
                    Located::new(v.to_string(), file, start, end)
                }
        rule int10_literal(file: SourceId) -> Located<EarlyLiteral>
            = _ start:position!() i:$(['0'..='9']*) end:position!() _
            {?
                Ok(Located::new(EarlyLiteral::Int(i.parse().or(Err("Int problem"))?),
                file, start, end))
            }

        rule int16_literal(file: SourceId) -> Located<EarlyLiteral>
            = _ start:position!() "0x" i:$(['0'..='9' | 'a'..='f' | 'A'..='F']*) end:position!() _
            {?
                Ok(Located::new(EarlyLiteral::Int(u64::from_str_radix(i, 16).or(Err("Int problem"))?),
                file, start, end))
            }

        rule int8_literal(file: SourceId) -> Located<EarlyLiteral>
            = _ start:position!() "0o" i:$(['0'..='7']*) end:position!() _
            {?
                Ok(Located::new(EarlyLiteral::Int(u64::from_str_radix(i, 8).or(Err("Int problem"))?),
                file, start, end))
            }

        rule int2_literal(file: SourceId) -> Located<EarlyLiteral>
            = _ start:position!() "0b" i:$(['0'..='1']*) end:position!() _
            {?
                Ok(Located::new(EarlyLiteral::Int(u64::from_str_radix(i, 2).or(Err("Int problem"))?),
                file, start, end))
            }

        rule int_literal(file: SourceId) -> Located<EarlyLiteral>
            = int2_literal(file)
            / int8_literal(file)
            / int16_literal(file)
            / int10_literal(file)

        rule bool_true_literal(file: SourceId) -> Located<EarlyStaticLiteral>
            = _ start:position!() "true" end:position!() _
            {
                Located::new(EarlyStaticLiteral::Bool(true), file, start, end)
            }

        rule bool_false_literal(file: SourceId) -> Located<EarlyStaticLiteral>
            = _ start:position!() "false" end:position!() _
            {
                Located::new(EarlyStaticLiteral::Bool(false), file, start, end)
            }

        rule bool_literal(file: SourceId) -> Located<EarlyStaticLiteral>
            = bool_true_literal(file)
            / bool_false_literal(file)

        rule static_literal(file: SourceId) -> Located<EarlyStaticLiteral>
            = bool_literal(file)
            / (i:int_literal(file) { i.map(|EarlyLiteral::Int(int)| EarlyStaticLiteral::Int(int) ) })

        rule literal(file: SourceId) -> Located<EarlyLiteral>
            = int_literal(file)

        rule incl_range(file: SourceId) -> Located<EarlyIndex>
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

        rule excl_range(file: SourceId) -> Located<EarlyIndex>
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

        rule index(file: SourceId) -> Located<EarlyIndex>
            = incl_range(file)
            / excl_range(file)
            / (e: static_expression(file) { e.map(|exp| EarlyIndex::Simple(exp)) })

        rule static_expression(file: SourceId) -> Located<EarlyStaticExpression> = precedence! {
                start:position!() "!" end:position!() _ x:@ {
                    let loc = x.get_loc();
                    Located::from_range(EarlyStaticExpression::MonOp {
                        operator: Located::empty_from_range(EarlyStaticMonOp::Not, start..end),
                        operand: Box::new(x),
                    }, file, loc.extend(start, end))
                }

                --

                start:position!() "-" end:position!() _ x:@ {
                    let loc = x.get_loc();
                    Located::from_range(EarlyStaticExpression::MonOp {
                        operator: Located::empty_from_range(EarlyStaticMonOp::Minus, start..end),
                        operand: Box::new(x),
                    }, file, loc.extend(start, end))
                }

                --

               x:(@) _ start:position!() "==" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::Equals, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "!=" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::NEquals, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

                --

               x:(@) _ start:position!() "+" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::Plus, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "-" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::Minus, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

                 --

               x:(@) _ start:position!() "*" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::Mult, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "/" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::Div, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "%" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::from_range(EarlyStaticExpression::BinOp {
                        operator: Located::empty_from_range(EarlyStaticBinOp::Modulo, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
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

        rule expression(file: SourceId) -> Located<EarlyExpression> = precedence! {
                start:position!() "~" end:position!() _ x:@ {
                    let loc = x.get_loc();
                    Located::from_range(EarlyExpression::MonOp {
                        operator: Located::empty_from_range(EarlyMonOp::BitNot, start..end),
                        operand: Box::new(x),
                    }, file, loc.extend(start, end))
                }

                 --

               x:(@) _ start:position!() "|" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::BitOr, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

                 --


               x:(@) _ start:position!() "&" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::BitAnd, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

               x:(@) _ start:position!() "^" end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::BitXOr, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

                --

               x:(@) _ start:position!() "." end:position!() _ y:@ {
                    let loc_x = x.get_loc();
                    let loc_y = y.get_loc();
                    Located::from_range(EarlyExpression::BinOp {
                        operator: Located::empty_from_range(EarlyBinOp::Concat, start..end),
                        lhs: Box::new(x),
                        rhs: Box::new(y),
                    }, file, loc_x.union(loc_y))
                }

                --

                 _ start:position!() builtin:"@"? builtin_end:position!() 
                   i:ident(file) s_args:("<" _ 
                   e:static_expression(file) ** "," _ ">" { e })?
                   "(" _ r_args:expression(file) ** "," _ ")" 
                   end:position!() _ {
                         println!("Parsing function call.");
                         Located::new(
                             EarlyExpression::FuncCall {
                                 func_name: i,
                                 static_params: s_args,
                                 runtime_params: r_args,
                                 builtin: Located::new(
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
                    Located::from_range(
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

        rule runtime_type(file: SourceId) -> Located<EarlyStaticExpression>
            = _ start:position!() "[" exp:static_expression(file) "]" end:position!() _
            {
                exp
            }

        rule runtime_type_annot(file: SourceId) -> Located<EarlyStaticExpression>
            = _ start:position!() ":" _ r:runtime_type(file) end:position!() _
            {
                r
            }

        rule runtime_arg(file: SourceId) -> Located<EarlyRuntimeArg>
            = _ start:position!() i:ident(file) typ:runtime_type_annot(file)? end:position!() _
            {
                Located::new(EarlyRuntimeArg { name: i, typ }, file, start, end)
            }

        rule runtime_arg_vec(file: SourceId) -> Located<Vec<Located<EarlyRuntimeArg>>>
            = start:position!() "(" args:runtime_arg(file) ** "," ")" end:position!()
            {
                Located::new(args, file, start, end)
            }

        rule static_type_bool(file:SourceId) -> Located<EarlyStaticBaseType>
            = _ start:position!() _ "bool" end:position!() _
            {
                Located::new(EarlyStaticBaseType::Bool, file, start, end)
            }

        rule static_type_int(file:SourceId) -> Located<EarlyStaticBaseType>
            = _ start:position!() _ "int" end:position!() _
            {
                Located::new(EarlyStaticBaseType::Int, file, start, end)
            }

        rule static_type_base(file: SourceId) -> Located<EarlyStaticBaseType>
            = static_type_int(file)
            / static_type_bool(file)

        rule static_type_refined(file: SourceId) -> Located<EarlyStaticType>
            = _ ":" _ start:position!() "{" base:(base:static_type_base(file) _ ":" { base })? _
            e:static_expression(file)? "}" end:position!() _
            {
                Located::new(
                    EarlyStaticType {
                        base,
                        refinement: e,
                    },
                    file,
                    start,
                    end,
                    )
            }

        rule static_type_simple(file: SourceId) -> Located<EarlyStaticType>
            = _ ":" _ start:position!() t:static_type_base(file) end:position!() _
            {
                Located::new(
                    EarlyStaticType {
                        base: Some(t),
                        refinement: None,
                    },
                    file,
                    start,
                    end,
                    )
            }

        rule static_type(file: SourceId) -> Option<Located<EarlyStaticType>>
            = s:static_type_simple(file) { Some(s) }
            / s:static_type_refined(file) { Some(s) }
            / { None }

        rule static_arg(file: SourceId) -> Located<EarlyStaticArg>
            = _ start:position!() i:ident(file) _ t:static_type(file) end:position!() _
            {
                Located::new(EarlyStaticArg { name: i, typ: t }, file, start, end)
            }

        rule static_arg_vec(file: SourceId) -> Located<Vec<Located<EarlyStaticArg>>>
            = _ start:position!() "<" _ a:static_arg(file) ** "," _ ">" end:position!()
            {
                Located::new(a, file, start, end)
            }

        rule lhs_ident(file: SourceId) -> EarlyStatementLhs
            = i:ident(file)
            {
                EarlyStatementLhs::Ident(i)
            }

        rule lhs_tuple(file: SourceId) -> EarlyStatementLhs
            = _ start:position!() "(" _ i:ident(file) ** "," _ ")" end:position!() _
            {
                EarlyStatementLhs::Tuple(Located::new(i, file, start, end))
            }

        rule lhs(file: SourceId) -> EarlyStatementLhs
            = lhs_tuple(file)
            / lhs_ident(file)

        rule statement_affect(file: SourceId) -> Located<EarlyStatement>
            = _ start:position!() l:lhs(file) "=" e:expression(file) ";" end:position!() _
            {
                Located::new(EarlyStatement::Affect { lhs: l, rhs: Box::new(e) }, file, start, end)
            }

        rule statement_if_then_else(file: SourceId) -> Located<EarlyStatement>
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

        rule statement(file: SourceId) -> Located<EarlyStatement>
            = statement_affect(file)
            / statement_if_then_else(file)

        rule block(file: SourceId) -> Located<EarlyBlock>
            = _ start:position!() b:statement(file)* end:position!() _
            {
                Located::new(b, file, start, end)
            }

        rule node(file: SourceId) -> Located<EarlyNode>
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
    fn complex_idents() {
        let code = r"
node f(a9A__: [1], __b: [n]) -> (c, d:[42]) {
    _0a = b;
}
        ";
        run(code);
    }

    #[test]
    #[should_panic]
    fn broken_ident() {
        let code = r"
node f(8a: [1], b: [n]) -> (c, d:[42]) {
    12_a = 1;
    123 = 2;
}
        ";
        run(code);
    }

    #[test]
    fn node_call() {
        let code = r"
node f(a: [1], b: [n]) -> (c, d:[42]) {
    a = f<n - 1, 12>(b, c[..=0]);
    b = @reg(a, b);
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

    #[test]
    fn refined_type() {
        let code = r"
node f<n: {int: n == 0}, m: {n != m}, o: {bool: o}>(a) -> (b) {
    b = a;
}
        ";
        run(code);
    }

    #[test]
    fn multiple_nodes() {
        let code = r"
node f<n>(a) -> (b) {
    b = a;
}

node g<m>() -> () {

}
        ";
        run(code);
    }
}
