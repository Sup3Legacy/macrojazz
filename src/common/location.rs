use super::source::*;

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Location {
    file: SourceId,
    range: std::ops::Range<usize>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Located<U: Clone, T: Clone> {
    pub inner: T,
    pub custom: U,
    pub loc: Location,
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

impl std::fmt::Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "range: {:?}", self.range)
    }
}

impl<U: Clone, T: Clone> Located<U, T> {
    pub fn __new(inner: T, custom: U, file: SourceId, start: usize, end: usize) -> Self {
        Self {
            inner,
            custom,
            loc: Location::new(file, start, end),
        }
    }

    pub fn __from_loc(inner: T, custom: U, loc: Location) -> Self {
        Self { inner, custom, loc }
    }

    pub fn __from_range(
        inner: T,
        custom: U,
        file: SourceId,
        range: std::ops::Range<usize>,
    ) -> Self {
        Self {
            inner,
            custom,
            loc: Location::from_range(file, range),
        }
    }

    pub fn empty_from_range(inner: T, custom: U, range: std::ops::Range<usize>) -> Self {
        Self {
            inner,
            custom,
            loc: Location::empty_from_range(range),
        }
    }

    pub fn get_inner(self) -> T {
        self.inner
    }

    pub fn get_loc(&self) -> Location {
        self.loc.clone()
    }

    // TODO: Also map the custom field
    pub fn map<V: Clone, F>(self, f: F) -> Located<U, V>
    where
        F: FnOnce(T) -> V,
    {
        Located {
            inner: f(self.inner),
            custom: self.custom,
            loc: self.loc,
        }
    }
}
