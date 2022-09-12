use super::*;




/// Type of a compiler report. Used to debug info
/// Not to be confused with source-bound warnings and errors,
/// that are handled by `ariadne`
#[derive(Clone, Copy, Debug)]
pub enum GlobalReportKind {
    Notice,
    Warning,
    Error,
}

pub struct CompilerReport {
    source: Option<SourceId>,
    kind: GlobalReportKind,
    help: Option<String>,
}

impl CompilerReport {
    pub fn print(&self) {
        println!("General report. WIP");
    }
}

#[macro_export]
macro_rules! single_label {
    ($type:ident, $code:expr, $color:ident, $source_path:expr, $source:expr,
     $loc_start:expr, $loc_end:expr, $header:expr, $message:expr) => {
        Report::build(ReportKind::$type, $source_path, $loc_start)
            .with_message($header)
            .with_code($code)
            .with_label(
                Label::new(($source_path, $loc_start..$loc_end))
                    .with_message($message)
                    .with_color(Color::$color),
            )
            .with_config(
                Config::default()
                    .with_cross_gap(false)
                    .with_compact(false)
                    .with_underlines(true)
                    .with_tab_width(4),
            )
            .finish()
            .print(($source_path, $source))
            .unwrap();
    };
}
