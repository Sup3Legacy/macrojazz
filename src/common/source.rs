use anyhow::Result;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct SourceId(usize);

impl SourceId {
    pub fn empty() -> Self {
        Self(0)
    }

    pub fn new(id: usize) -> Self {
        Self(id)
    }
}

// TODO: Implement ariadne::Cache for this struct.
pub struct SourceCache {
    sources: Vec<(String, String)>,
}

impl SourceCache {
    /// Creates a new [`SourceCache`].
    pub fn new() -> Self {
        Self {
            sources: Vec::new(),
        }
    }

    pub fn _len(&self) -> usize {
        self.sources.len()
    }

    pub fn add_from_path(&mut self, path: String) -> Result<()> {
        let source = std::fs::read_to_string(path.clone())?;
        self.sources.push((path, source));
        Ok(())
    }

    pub fn get_source(&'_ self, id: SourceId) -> Option<(&'_ str, &'_ str)> {
        self.sources
            .get(id.0)
            .map(|(p, s)| (p.as_str(), s.as_str()))
    }

    pub fn iter(&'_ self) -> SourceIterator<'_> {
        SourceIterator {
            cache: self,
            index: 0,
        }
    }
}

impl Default for SourceCache {
    fn default() -> Self {
        Self::new()
    }
}

pub struct SourceIterator<'a> {
    cache: &'a SourceCache,
    index: usize,
}

impl<'a> Iterator for SourceIterator<'a> {
    type Item = (SourceId, (&'a str, &'a str));

    fn next(&mut self) -> Option<Self::Item> {
        self.cache.get_source(SourceId::new(self.index)).map(|s| {
            let index = self.index;
            self.index += 1;
            (SourceId::new(index), s)
        })
    }
}
