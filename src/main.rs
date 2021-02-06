use std::io;
use std::io::prelude::*;
use std::fs::File;

fn main() -> io::Result<()> {
    let f = File::open("examples/odd_even.ik")?;
    let mut reader = io::BufReader::new(f);
    let mut contents = String::new();
    reader.read_to_string(&mut contents)?;

    let tokens = tokenize(contents);
    println!("tokens: {:?}", tokens);
    Ok(())
}

fn name_token(name: String) -> Token {
    match name.as_str() {
        "class"    => Token::KeywordClass,
        "else"     => Token::KeywordElse,
        "enum"     => Token::KeywordEnum,
        "fn"       => Token::KeywordFn,
        "for"      => Token::KeywordFor,
        "if"       => Token::KeywordIf,
        "import"   => Token::KeywordImport,
        "instance" => Token::KeywordInstance,
        "let"      => Token::KeywordLet,
        "match"    => Token::KeywordMatch,
        "package"  => Token::KeywordPackage,
        "struct"   => Token::KeywordStruct,
        "type"     => Token::KeywordType,
        "where"    => Token::KeywordWhere,
        "while"    => Token::KeywordWhile,
        "with"     => Token::KeywordWith,
        _          => Token::ValueName(name),
    }
}

fn tokenize(text: String) -> io::Result<Vec<(Token, Location)>> {
    let mut tokens = vec![];
    let mut current = String::new();
    // Location where the current token started
    let mut last_location = Location::new();
    // Location of the current character
    let mut location = Location::new();

    enum State {
        Start,
        Operator,
        Slash,
        LineComment,
        BlockComment,
        BlockCommentStar,
        InName, // or keyword
        InType,
        InInteger,
        InFloat,
        InString,
        InStringEscape,
        Unknown,
    }

    use State::*;

    let mut state = State::Start;

    for c in text.chars() {
        location.update(c);

        // This section should `continue;` if it ate the current character
        match state {
            Start => {
                debug_assert!(current.is_empty());
            },
            Operator => {
                debug_assert!(!current.is_empty());
                let previous = current.chars().next().unwrap();
                match previous {
                    '=' => {
                        if c == '=' {
                            tokens.push((Token::DoubleEquals, last_location));
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push((Token::Equals, last_location));
                        }
                    },
                    '!' => {
                        if c == '=' {
                            tokens.push((Token::NotEquals, last_location));
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push((Token::Bang, last_location));
                        }
                    },
                    '<' => {
                        if c == '=' {
                            tokens.push((Token::LessEquals, last_location));
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push((Token::Less, last_location));
                        }
                    },
                    '>' => {
                        if c == '=' {
                            tokens.push((Token::GreaterEquals, last_location));
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push((Token::Greater, last_location));
                        }
                    },
                    '&' => {
                        if c == '&' {
                            tokens.push((Token::DoubleAnd, last_location));
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push((Token::SingleAnd, last_location));
                        }
                    },
                    '|' => {
                        if c == '|' {
                            tokens.push((Token::DoubleOr, last_location));
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push((Token::SingleOr, last_location));
                        }
                    },
                    _ => {
                        panic!("unexpected prev char in operator");
                    },
                }
            },
            Slash => {
                debug_assert!(current.is_empty());
                if c == '/' {
                    state = LineComment;
                    continue;
                } else if c == '*' {
                    state = BlockComment;
                    continue;
                } else {
                    tokens.push((Token::Slash, last_location));
                }
            },
            BlockComment => {
                if c == '*' {
                    state = BlockCommentStar;
                } else {
                    current.push(c);
                }
                continue;
            },
            BlockCommentStar => {
                if c == '/' {
                    tokens.push((Token::BlockComment(current), last_location));
                    current = String::new();
                    state = Start;
                } else if c == '*' {
                    current.push(c);
                    state = BlockCommentStar;
                } else {
                    current.push('*');
                    current.push(c);
                    state = BlockComment;
                }
                continue;
            },
            LineComment => {
                if c == '\n' {
                    tokens.push((Token::LineComment(current), last_location));
                } else {
                    current.push(c);
                    continue;
                }
            },
            InName => {
                debug_assert!(!current.is_empty());
                if c.is_alphanumeric() || c == '_' {
                    current.push(c);
                    continue;
                }
                tokens.push((name_token(current), last_location));
            },
            InType => {
                debug_assert!(!current.is_empty());
                if c.is_alphanumeric() || c == '_' {
                    current.push(c);
                    continue;
                }
                tokens.push((Token::TypeName(current), last_location));
            },
            InInteger => {
                debug_assert!(!current.is_empty());
                if c.is_ascii_digit() {
                    current.push(c);
                    continue;
                } else if c == '.' {
                    current.push(c);
                    state = InFloat;
                    continue;
                }
                let n = current.parse();
                tokens.push((Token::IntLiteral(n.unwrap()), last_location));
            },
            InFloat => {
                debug_assert!(!current.is_empty());
                if c.is_ascii_digit() {
                    current.push(c);
                    continue;
                }
                // TODO: Handle exponents
                let n = current.parse();
                tokens.push((Token::FloatLiteral(n.unwrap()), last_location));
            },
            InString => {
                debug_assert!(!current.is_empty());
                current.push(c);
                if c == '"' {
                    tokens.push((Token::StringLiteral(current), last_location));
                    current = String::new();
                    state = Start;
                } else if c == '\\' {
                    state = InStringEscape;
                }
                continue;
            },
            InStringEscape => {
                current.push(c);
                state = InString;
            },
            Unknown => {
                debug_assert!(!current.is_empty());
                if !(c.is_whitespace() || c == '(' || c == ')'
                     || c == '[' || c == ']' || c == '{' || c == '}') {
                    current.push(c);
                    continue;
                }
                tokens.push((Token::Unknown(current), last_location));
            },
        }

        current = String::new();
        state = Start;

        // If the previous section didn't handle the current character, handle it here
        match c {
            '\n' => {
                tokens.push((Token::Newline, location));
            },
            _ if c.is_whitespace() => {
                // Ignore it
            },
            '.' => tokens.push((Token::Dot, location)),
            ',' => tokens.push((Token::Comma, location)),
            ':' => tokens.push((Token::Colon, location)),
            '[' => tokens.push((Token::LBracket, location)),
            ']' => tokens.push((Token::RBracket, location)),
            '(' => tokens.push((Token::LParen, location)),
            ')' => tokens.push((Token::RParen, location)),
            '{' => tokens.push((Token::LBrace, location)),
            '}' => tokens.push((Token::RBrace, location)),
            '+' => tokens.push((Token::Plus, location)),
            '-' => tokens.push((Token::Minus, location)),
            '*' => tokens.push((Token::Star, location)),
            '/' => {
                last_location = location;
                state = Slash;
            },
            '%' => tokens.push((Token::Percent, location)),
            '~' => tokens.push((Token::Tilda, location)),
            '^' => tokens.push((Token::Caret, location)),
            '=' | '!' | '>' | '<' | '&' | '|' => {
                current.push(c);
                last_location = location;
                state = Operator;
            },
            '"' => {
                current.push(c);
                last_location = location;
                state = InString;
            },
            _ if c.is_alphabetic() || c == '_' => {
                last_location = location;
                current.push(c);
                if c.is_uppercase() {
                    state = InType;
                } else {
                    state = InName;
                }
            },
            _ if c.is_ascii_digit() => {
                last_location = location;
                current.push(c);
                state = InInteger;
            },
            _ => {
                last_location = location;
                current.push(c);
                state = Unknown;
            }
        }
    }

    if !current.is_empty() {
        // push the last token
        match state {
            Start => {
                panic!("current should always empty in the starting state");
            },
            Operator => {
                debug_assert!(!current.is_empty());
                let previous = current.chars().next().unwrap();
                match previous {
                    '=' => tokens.push((Token::Equals, last_location)),
                    '!' => tokens.push((Token::Bang, last_location)),
                    '<' => tokens.push((Token::Less, last_location)),
                    '>' => tokens.push((Token::Greater, last_location)),
                    '&' => tokens.push((Token::SingleAnd, last_location)),
                    '|' => tokens.push((Token::SingleOr, last_location)),
                    _ => {
                        panic!("unexpected prev char in operator");
                    },
                }
            },
            Slash => {
                tokens.push((Token::Slash, last_location));
            },
            LineComment => {
                tokens.push((Token::LineComment(current), last_location));
            },
            BlockComment | BlockCommentStar => {
                // Block comment was never closed
                tokens.push((Token::Unknown(current), last_location));
            },
            InName => {
                tokens.push((name_token(current), last_location));
            },
            InType => {
                tokens.push((Token::TypeName(current), last_location));
            },
            InInteger => {
                let n = current.parse();
                tokens.push((Token::IntLiteral(n.unwrap()), last_location));
            },
            InFloat => {
                let n = current.parse();
                tokens.push((Token::FloatLiteral(n.unwrap()), last_location));
            },
            InString | InStringEscape => {
                // Unclosed string
                tokens.push((Token::Unknown(current), last_location));
            },
            Unknown => {
                tokens.push((Token::Unknown(current), last_location));
            },
        }
    }

    Ok(tokens)
}

#[derive(Debug)]
enum Token {
    // Bad tokens:
    Unknown(String),

    Colon,
    Comma,
    Dot,
    Newline,

    Bang,
    Caret,
    DoubleAnd,
    DoubleEquals,
    DoubleOr,
    Equals,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    Minus,
    NotEquals,
    Percent,
    Plus,
    SingleAnd,
    SingleOr,
    Slash,
    Star,
    Tilda,

    LBrace,
    LBracket,
    LParen,
    RBrace,
    RBracket,
    RParen,

    KeywordClass,
    KeywordElse,
    KeywordEnum,
    KeywordFn,
    KeywordFor,
    KeywordIf,
    KeywordImport,
    KeywordInstance,
    KeywordLet,
    KeywordMatch,
    KeywordPackage,
    KeywordStruct,
    KeywordType,
    KeywordWhere,
    KeywordWhile,
    KeywordWith,

    ValueName(String),
    TypeName(String),

    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),

    // Idea: These could contain start/end locations (or just end locations?)
    // and expect callers to find the value in the file
    LineComment(String),
    BlockComment(String),
}

#[derive(Debug, Clone, Copy)]
struct Location {
    col: u32,
    line: u32,
}

impl Location {
    fn new() -> Self {
        Self{col: 0, line: 0}
    }

    fn update(&mut self, c: char) {
        if c == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
    }
}
