#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub col: u32,
    pub line: u32,
}

impl Location {
    pub fn new() -> Self {
        Self{col: 0, line: 0}
    }

    pub fn update(&self, c: char) -> Self {
        let mut new_location = *self;
        if c == '\n' {
            new_location.line += 1;
            new_location.col = 0;
        } else {
            new_location.col += 1;
        }
        new_location
    }

    pub fn is_before(&self, other: Location) -> bool {
        if other.line > self.line {
            return true;
        } else if other.line < self.line {
            return false;
        }

        other.col > self.col
    }
}
