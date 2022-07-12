#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct SrcId(usize);

impl SrcId {
    pub fn empty() -> Self {
        Self(0)
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Location {
    file: SrcId,
    range: std::ops::Range<usize>,
}

impl std::fmt::Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "range: {:?}", self.range)
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Located<T> {
    inner: T,
    loc: Location,
}

impl Location {
    pub fn new(file: SrcId, start: usize, end: usize) -> Self {
        Self {
            file,
            range: start..end,
        }
    }

    pub fn empty(start: usize, end: usize) -> Self {
        Self {
            file: SrcId::empty(),
            range: start..end,
        }
    }

    pub fn from_range(file: SrcId, range: std::ops::Range<usize>) -> Self {
        Self { file, range }
    }

    pub fn empty_from_range(range: std::ops::Range<usize>) -> Self {
        Self {
            file: SrcId::empty(),
            range,
        }
    }

    pub fn get_range(self) -> std::ops::Range<usize> {
        self.range
    }
}

impl<T> Located<T> {
    pub fn new(inner: T, file: SrcId, start: usize, end: usize) -> Self {
        Self {
            inner,
            loc: Location::new(file, start, end),
        }
    }

    pub fn from_range(inner: T, file: SrcId, range: std::ops::Range<usize>) -> Self {
        Self {
            inner,
            loc: Location::from_range(file, range),
        }
    }

    pub fn empty_from_range(inner: T, range: std::ops::Range<usize>) -> Self {
        Self {
            inner,
            loc: Location::empty_from_range(range),
        }
    }

    pub fn get_inner(self) -> T {
        self.inner
    }

    pub fn map<U, F>(self, f: F) -> Located<U>
    where
        F: FnOnce(T) -> U,
    {
        Located {
            inner: f(self.inner),
            loc: self.loc,
        }
    }
}

impl chumsky::Span for Location {
    type Offset = usize;
    type Context = SrcId;

    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self {
            file: context,
            range,
        }
    }
    fn context(&self) -> Self::Context {
        self.file
    }

    fn start(&self) -> Self::Offset {
        self.range.start
    }

    fn end(&self) -> Self::Offset {
        self.range.end
    }
}

impl ariadne::Span for Location {
    type SourceId = SrcId;

    fn source(&self) -> &SrcId {
        &self.file
    }
    fn start(&self) -> usize {
        self.range.start
    }
    fn end(&self) -> usize {
        self.range.end
    }
}
