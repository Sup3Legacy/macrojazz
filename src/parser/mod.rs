mod ast;

use crate::common::location::*;
use ast::*;
use anyhow::{anyhow, Context, Result};
use std::{sync::Arc, path::PathBuf};
use ariadne::{Source, FileCache};

extern crate peg;

peg::parser! {
    grammar macrojazz() for Vec<String> {
        // Whitespace
        rule _ = [' ' | '\n' | '\t']* comment()*

        rule comment() -> ()
            = "//" [^'\n']*

        pub rule program(file: &Arc<Source>) -> EarlyProgram
            = _ d:node_declaration(file)* _ { d }

        rule ident(file: &Arc<Source>) -> Located<EarlyIdentifier>
            = _ start:position!()
                !"node"
                !"if"
                !"else"
                !"end"
                v:$(['a'..='z' | 'A'..='Z']*)
                end:position!() _
                {
                    Located::new(v.to_string(), file, start, end)
                }
        rule int_literal(file: &Arc<Source>) -> Located<EarlyLiteral>
            = _ start:position!() i:$(['0'..='9']) end:position!() _
            {?
                Ok(Located::new(EarlyLiteral::Int(i.parse().or(Err("Int problem"))?),
                file, start, end))
            }

        rule bool_true_literal(file: &Arc<Source>) -> Located<EarlyLiteral>
            = _ start:position!() "true" end:position!() _
            {
                Located::new(EarlyLiteral::Bool(true), file, start, end)
            }

        rule bool_false_literal(file: &Arc<Source>) -> Located<EarlyLiteral>
            = _ start:position!() "false" end:position!() _
            {
                Located::new(EarlyLiteral::Bool(false), file, start, end)
            }

        rule bool_literal(file: &Arc<Source>) -> Located<EarlyLiteral>
            = bool_true_literal(file)
            / bool_false_literal(file)

        rule literal(file: &Arc<Source>) -> Located<EarlyLiteral>
            = bool_literal(file)
            / int_literal(file)

        // rule node_declaration(file: &Arc<Source>) -> Located:Whitespace
    }
}

fn parse_single_source(file_ref: &Arc<Source>, file_source: String,)

pub fn parse_sources(sources: Vec<PathBuf>) -> Result<(EarlyProgram, FileCache)> {
    let mut filecache = FileCache::default();

    for src in sources {
        
    }

    todo!()
}
