use chumsky::span::Span;

/// Mock file type
/// TODO
pub type File = usize;

#[derive(Debug, Clone)]
pub struct Location {
    range: std::ops::Range<usize>,
}

#[derive(Debug)]
pub struct Located<T> {
    inner: T,
    loc: Location,
}

impl Location {
    pub fn new(start: usize, end: usize) -> Self {
        Self { range: start..end }
    }

    pub fn from_range(range: std::ops::Range<usize>) -> Self {
        Self {range}
    }
}

impl<T> Located<T> {
    pub fn new(inner: T, start: usize, end: usize) -> Self {
        Self {
            inner,
            loc: Location::new(start, end),
        }
    }

    pub fn from_range(inner: T, range: std::ops::Range<usize>) -> Self {
        Self {
            inner,
            loc: Location::from_range(range),
        }
    }
}

impl Span for Location {
    type Offset = usize;
    type Context = ();

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self { range }
    }
    fn context(&self) -> Self::Context {}

    fn start(&self) -> Self::Offset {
        self.range.start
    }

    fn end(&self) -> Self::Offset {
        self.range.end
    }
}
