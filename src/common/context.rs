use super::*;
use crate::{parser::ast::EarlyProgram, single_label, typing};
use anyhow::Result;
use ariadne::{Color, Config, Fmt, Label, Report, ReportKind, Source};
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

#[derive(Default)]
pub enum Program {
    #[default]
    Null,
    Parsed(EarlyProgram),
    Error(CompileError),
}

impl Program {
    pub fn take_parsed(&mut self) -> EarlyProgram {
        let value = std::mem::take(self);
        match value {
            Program::Parsed(p) => p,
            _ => unreachable!()
        }
    }
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
        let reports = Vec::new();
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
                        format!("Expected {}", expected.to_string().as_str().fg(Color::Red))
                    );

                    success = false;
                }
            }
        }

        if success {
            self.program = Program::Parsed(program);
            Ok(reports)
        } else {
            Err(reports)
        }
    }

    pub fn typecheck(&mut self) -> Result<(), ()> {
        let parsed = self.program.take_parsed();
        // TODO: build environment of nodes for instantiation typechecking
        for node in parsed {
            typing::expr::type_check_node(node);
        }
        Ok(())
    }

    pub fn run(&mut self) {
        self.parse();
    }
}
