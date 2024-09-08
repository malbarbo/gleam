pub(crate) struct IndexGenerator<T> {
    pub(crate) items: Vec<T>,
    pub(crate) offset: u32,
}

impl<T> IndexGenerator<T> {
    pub(crate) fn new() -> Self {
        Self {
            items: vec![],
            offset: 0,
        }
    }

    pub(crate) fn with_offset(offset: u32) -> Self {
        Self {
            items: vec![],
            offset,
        }
    }

    pub(crate) fn new_index(&mut self, item: T) -> u32 {
        let i = self.items.len() as u32;
        self.items.push(item);
        i + self.offset
    }

    pub(crate) fn len(&self) -> u32 {
        self.items.len() as u32
    }

    pub(crate) fn get(&self, index: u32) -> Option<&T> {
        self.items.get((index - self.offset) as usize)
    }
}
