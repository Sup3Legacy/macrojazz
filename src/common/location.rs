use ariadne::Source;
use std::sync::Arc;

#[derive(Clone)]
pub struct Location {
    file: Arc<Source>,
    start: usize,
    end: usize,
}

impl std::fmt::Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        f.debug_struct("Location")
            .field("start", &self.start)
            .field("end", &self.end)
            .finish()
    }
}

#[derive(Debug)]
pub struct Located<T> {
    inner: T,
    loc: Location,
}

impl Location {
    pub fn new(file: &Arc<Source>, start: usize, end: usize) -> Self {
        Self {
            file: file.clone(),
            start,
            end,
        }
    }
}

impl<T> Located<T> {
    pub fn new(inner: T, file: &Arc<Source>, start: usize, end: usize) -> Self {
        Self {
            inner,
            loc: Location::new(file, start, end),
        }
    }
}
