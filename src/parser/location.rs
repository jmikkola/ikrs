use std::cmp::{Ordering, PartialOrd, Ord};

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

    #[cfg(test)]
    pub fn is_before(&self, other: Location) -> bool {
        if other.line > self.line {
            return true;
        } else if other.line < self.line {
            return false;
        }

        other.col > self.col
    }
}

impl ToString for Location {
    fn to_string(&self) -> String {
        format!("line {}, column {}", self.line + 1, self.col + 1)
    }
}

impl PartialOrd for Location {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Location {
    fn cmp(&self, other: &Self) -> Ordering {
        self.line.cmp(&other.line).then(self.col.cmp(&other.col))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ordering() {
        let l0c0 = Location{line: 0, col: 0};
        let l0c3 = Location{line: 0, col: 3};
        let l2c0 = Location{line: 2, col: 0};
        let l2c3 = Location{line: 2, col: 3};

        use Ordering::*;

        assert_eq!(Equal, l0c0.cmp(&l0c0));
        assert_eq!(Less, l0c0.cmp(&l0c3));
        assert_eq!(Less, l0c0.cmp(&l2c0));
        assert_eq!(Less, l0c0.cmp(&l2c3));

        assert_eq!(Greater, l0c3.cmp(&l0c0));
        assert_eq!(Equal, l0c3.cmp(&l0c3));
        assert_eq!(Less, l0c3.cmp(&l2c0));
        assert_eq!(Less, l0c3.cmp(&l2c3));

        assert_eq!(Greater, l2c0.cmp(&l0c0));
        assert_eq!(Greater, l2c0.cmp(&l0c3));
        assert_eq!(Equal, l2c0.cmp(&l2c0));
        assert_eq!(Less, l2c0.cmp(&l2c3));

        assert_eq!(Greater, l2c3.cmp(&l0c0));
        assert_eq!(Greater, l2c3.cmp(&l0c3));
        assert_eq!(Greater, l2c3.cmp(&l2c0));
        assert_eq!(Equal, l2c3.cmp(&l2c3));
    }
}
