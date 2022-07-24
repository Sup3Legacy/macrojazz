use super::*;
use crate::parser::ast::EarlyProgram;
use anyhow::Result;
use ariadne::{Color, Config, Label, Report, ReportKind, Source};
use peg;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum CompileMode {
    ParseOnly,
    TypeOnly,
    CheckOnly,
    Full,
}

pub enum CompileError {
    ParsingError,
}

impl From<CompileError> for usize {
    fn from(c: CompileError) -> Self {
        match c {
            CompileError::ParsingError => 1,
        }
    }
}

pub enum Program {
    Null,
    Parsed(EarlyProgram),
    Error(CompileError),
}

pub struct CompileContext {
    sources: SourceCache,
    mode: CompileMode,
    program: Program,
}

impl CompileContext {
    pub fn new(mode: CompileMode) -> Self {
        Self {
            sources: SourceCache::new(),
            mode,
            program: Program::Null,
        }
    }

    pub fn add_from_path(&mut self, path: String) -> Result<()> {
        self.sources.add_from_path(path)
    }

    pub fn parse(&mut self) {
        let mut program: EarlyProgram = vec![];

        for (idx, (path, source)) in self.sources.iter() {
            let parsing_res = crate::parser::parse_single(source, idx);
            match parsing_res {
                Ok(mut parsed_program) => program.append(&mut parsed_program),
                Err(peg::error::ParseError { location, expected }) => {
                    self.program = Program::Error(CompileError::ParsingError);
                    Report::build(ReportKind::Error, path, location.offset)
                        .with_code(CompileError::ParsingError as usize)
                        .with_message("Parsing")
                        .with_label(
                            Label::new((path, location.offset..location.offset + 1))
                                .with_message(format!("Expected {}", expected))
                                .with_color(Color::Red),
                        )
                        .with_config(
                            Config::default()
                                .with_cross_gap(false)
                                .with_compact(false)
                                .with_underlines(true)
                                .with_tab_width(4),
                        )
                        .with_help("Check grammar specification (TBD)")
                        .finish()
                        .print((path, Source::from(source)))
                        .unwrap();
                }
            }
        }

        // TODO: mutate program state
    }

    pub fn run(&mut self) {
        self.parse();
    }
}
