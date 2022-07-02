use codemap;
use std::sync::Arc;

#[derive(Debug)]
pub struct Location {
    file: Arc<codemap::File>,
    start: usize,
    end: usize,
}

#[derive(Debug)]
pub struct Located<T> {
    inner: T,
    loc: Location,
}

impl Location {
    pub fn new(file: &Arc<codemap::File>, start: usize, end: usize) -> Self {
        Self {
            file: file.clone(),
            start,
            end,
        }
    }
}

impl<T> Located<T> {
    pub fn new(inner: T, file: &Arc<codemap::File>, start: usize, end: usize) -> Self {
        Self {
            inner,
            loc: Location::new(file, start, end),
        }
    }
}
