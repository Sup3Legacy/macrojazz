use super::*;
use crate::{parser::ast::EarlyProgram, single_label};
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

    pub fn iter_sources(&'_ self) -> SourceIterator<'_> {
        self.sources.iter()
    }

    pub fn parse(&mut self) -> Result<Vec<CompilerReport>, Vec<CompilerReport>> {
        let mut success = true;
        let mut reports = Vec::new();
        let mut program: EarlyProgram = vec![];

        for (idx, (path, source)) in self.sources.iter() {
            let parsing_res = crate::parser::parse_single(source, idx);
            match parsing_res {
                Ok(mut parsed_program) => program.append(&mut parsed_program),
                Err(peg::error::ParseError { location, expected }) => {
                    single_label!(
                        Error,
                        3,
                        Red,
                        path,
                        Source::from(source),
                        location.offset,
                        location.offset + 1,
                        "Parsing",
                        format!("Expected {}", expected)
                    );

                    success = false;
                }
            }
        }

        if success {
            Ok(reports)
        } else {
            Err(reports)
        }
    }

    pub fn run(&mut self) {
        self.parse();
    }
}
