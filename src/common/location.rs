use super::source::*;

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Location {
    file: SourceId,
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
    pub fn new(file: SourceId, start: usize, end: usize) -> Self {
        Self {
            file,
            range: start..end,
        }
    }

    pub fn _empty(start: usize, end: usize) -> Self {
        Self {
            file: SourceId::empty(),
            range: start..end,
        }
    }

    pub fn from_range(file: SourceId, range: std::ops::Range<usize>) -> Self {
        Self { file, range }
    }

    pub fn empty_from_range(range: std::ops::Range<usize>) -> Self {
        Self {
            file: SourceId::empty(),
            range,
        }
    }

    pub fn get_range(self) -> std::ops::Range<usize> {
        self.range
    }

    pub fn extend(self, start: usize, end: usize) -> std::ops::Range<usize> {
        self.range.start.min(start)..(self.range.end.max(end))
    }

    pub fn union(self, other: Self) -> std::ops::Range<usize> {
        (self.range.start.min(other.range.start))..(self.range.end.min(other.range.end))
    }
}

impl<T> Located<T> {
    pub fn new(inner: T, file: SourceId, start: usize, end: usize) -> Self {
        Self {
            inner,
            loc: Location::new(file, start, end),
        }
    }

    pub fn from_range(inner: T, file: SourceId, range: std::ops::Range<usize>) -> Self {
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

    pub fn get_loc(&self) -> Location {
        self.loc.clone()
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
