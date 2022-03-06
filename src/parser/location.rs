#![allow(clippy::comparison_chain)]

use std::cmp::{Ord, Ordering, PartialOrd, min};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    pub col: u32,
    pub line: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Region {
    pub start: Location,
    pub end: Location,
}

// Used to describe a part of a file that should be shown
#[derive(Debug)]
pub struct DisplaySelection {
    // Region of text to highlight
    pub highlight: Region,
    // How many lines before and after to also render
    context: u32,
}

impl Location {
    pub fn new() -> Self {
        Self { col: 0, line: 0 }
    }

    pub fn left(&self) -> Self {
        debug_assert!(self.col > 0);
        Self {
            col: self.col - 1,
            line: self.line,
        }
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

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, column {}", self.line + 1, self.col)
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

impl Region {
    pub fn new(start: Location, end: Location) -> Self {
        Region {start, end}
    }

    // this trusts you that you haven't specified a length that goes past the end of the line
    pub fn for_word(start: Location, length: usize) -> Self {
        let end = Location{line: start.line, col: start.col + (length as u32)};
        Region::new(start, end)
    }

    pub fn to_display_selection(self, context: u32) -> DisplaySelection {
        DisplaySelection::new(self, context)
    }
}

impl std::fmt::Display for Region {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "from {} to {}", self.start, self.end)
    }
}

impl DisplaySelection {
    pub fn new(highlight: Region, context: u32) -> Self {
        DisplaySelection{highlight, context}
    }

    pub fn render_selection(&self, file: &str) -> String {
        let lines: Vec<String> = file.lines().map(|l| l.to_string()).collect();

        let mut result = String::new();

        // copy starting context
        let start = self.highlight.start.line;
        for line in (start - min(start, self.context))..start {
            result += lines[line as usize].as_str();
            result += "\n";
        }

        // copy selected region and highlight it
        let end = self.highlight.end.line;
        for line in start..=end {
            result += lines[line as usize].as_str();
            result += "\n";

            // Janky hack for now: assume that the highlight is on a single line
            for _ in 0..self.highlight.start.col {
                result += " ";
            }
            for _ in self.highlight.start.col..self.highlight.end.col {
                result += "^";
            }
            result += "\n";
        }

        // copy ending context
        let last_line = min(end + self.context, lines.len() as u32 - 1);
        for line in (end + 1)..=last_line {
            result += lines[line as usize].as_str();
            result += "\n";
        }

        result
    }
}

impl std::fmt::Display for DisplaySelection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.highlight)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    const EXAMPLE_FILE: &str = r#"
        fn main():
            let a = 1
            let b = 0
            while a < 200:
                print(to_string(a))
                print("\n")
                let temp = a
                a = a + b
                b = temp
"#;

    // Clean up leading newline
    fn trim(text: &str) -> &str {
        let to_trim: &[_] = &['\n'];
        text.trim_start_matches(to_trim)
    }

    #[test]
    fn test_render_selection_word() {
        let start = Location{line: 3, col: 12};
        let region = Region::for_word(start, 5);
        let selection = region.to_display_selection(2);


        let result = selection.render_selection(trim(EXAMPLE_FILE));

        let expected = r#"
            let a = 1
            let b = 0
            while a < 200:
            ^^^^^
                print(to_string(a))
                print("\n")
"#;

        assert_eq!(trim(expected).to_string(), result);
    }

    #[test]
    fn test_ordering() {
        let l0c0 = Location { line: 0, col: 0 };
        let l0c3 = Location { line: 0, col: 3 };
        let l2c0 = Location { line: 2, col: 0 };
        let l2c3 = Location { line: 2, col: 3 };

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
