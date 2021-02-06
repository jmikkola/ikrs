#[derive(Debug, Clone, Copy)]
pub struct Location {
    col: u32,
    line: u32,
}

impl Location {
    pub fn new() -> Self {
        Self{col: 0, line: 0}
    }

    pub fn update(&mut self, c: char) {
        if c == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
    }
}
