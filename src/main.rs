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

fn tokenize(text: String) -> io::Result<Vec<Token>> {
    let mut tokens = vec![];
    let mut current = String::new();

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
        InString,
        Unknown,
    }

    use State::*;

    let mut state = State::Start;

    for c in text.chars() {
        // This section should `continue;` if it ate the current character
        match state {
            Start => {
                assert!(current.is_empty());
            },
            Operator => {
                assert!(!current.is_empty());
                let previous = current.chars().next().unwrap();
                match previous {
                    '=' => {
                        if c == '=' {
                            tokens.push(Token::DoubleEquals);
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push(Token::Equals);
                        }
                    },
                    '!' => {
                        if c == '=' {
                            tokens.push(Token::NotEquals);
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push(Token::Bang);
                        }
                    },
                    '<' => {
                        if c == '=' {
                            tokens.push(Token::LessEquals);
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push(Token::Less);
                        }
                    },
                    '>' => {
                        if c == '=' {
                            tokens.push(Token::GreaterEquals);
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push(Token::Greater);
                        }
                    },
                    '&' => {
                        if c == '&' {
                            tokens.push(Token::DoubleAnd);
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push(Token::SingleAnd);
                        }
                    },
                    '|' => {
                        if c == '|' {
                            tokens.push(Token::DoubleOr);
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push(Token::SingleOr);
                        }
                    },
                    _ => {
                        panic!("unexpected prev char in operator");
                    },
                }
            },
            Slash => {
                assert!(current.is_empty());
                // TODO: Block comment
                if c == '/' {
                    state = LineComment;
                    continue;
                } else if c == '*' {
                    state = BlockComment;
                    continue;
                } else {
                    tokens.push(Token::Slash);
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
                    tokens.push(Token::BlockComment(current));
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
                    tokens.push(Token::LineComment(current));
                } else {
                    current.push(c);
                    continue;
                }
            },
            InName => {
                assert!(!current.is_empty());
                if c.is_alphanumeric() || c == '_' {
                    current.push(c);
                    continue;
                }
                tokens.push(name_token(current));
            },
            InType => {
                assert!(!current.is_empty());
                if c.is_alphanumeric() || c == '_' {
                    current.push(c);
                    continue;
                }
                tokens.push(Token::TypeName(current));
            },
            InInteger => {
                assert!(!current.is_empty());
                if c.is_ascii_digit() {
                    current.push(c);
                    continue;
                }
                // TODO: Handle floats
                let n = i64::from_str_radix(current.as_str(), 10);
                tokens.push(Token::IntLiteral(n.unwrap()));
            },
            InString => {
                assert!(!current.is_empty());
                current.push(c);
                if c == '"' {
                    tokens.push(Token::StringLiteral(current));
                    current = String::new();
                    state = Start;
                }
                // TODO: Handle escape characters
                continue;
            },
            Unknown => {
                assert!(!current.is_empty());
                if !(c.is_whitespace() || c == '(' || c == ')'
                     || c == '[' || c == ']' || c == '{' || c == '}') {
                    current.push(c);
                    continue;
                }
                tokens.push(Token::Unknown(current));
            },
        }

        current = String::new();
        state = Start;

        // If the previous section didn't handle the current character, handle it here
        match c {
            '\n' => {
                tokens.push(Token::Newline);
            },
            _ if c.is_whitespace() => {
                // Ignore it
            },
            '.' => tokens.push(Token::Dot),
            ',' => tokens.push(Token::Comma),
            ':' => tokens.push(Token::Colon),
            '[' => tokens.push(Token::LBracket),
            ']' => tokens.push(Token::RBracket),
            '(' => tokens.push(Token::LParen),
            ')' => tokens.push(Token::RParen),
            '{' => tokens.push(Token::LBrace),
            '}' => tokens.push(Token::RBrace),
            '+' => tokens.push(Token::Plus),
            '-' => tokens.push(Token::Minus),
            '*' => tokens.push(Token::Star),
            '/' => {
                state = Slash;
            },
            '%' => tokens.push(Token::Percent),
            '~' => tokens.push(Token::Tilda),
            '^' => tokens.push(Token::Caret),
            '=' | '!' | '>' | '<' | '&' | '|' => {
                current.push(c);
                state = Operator;
            },
            '"' => {
                current.push(c);
                state = InString;
            },
            _ if c.is_alphabetic() || c == '_' => {
                current.push(c);
                if c.is_uppercase() {
                    state = InType;
                } else {
                    state = InName;
                }
            },
            _ if c.is_ascii_digit() => {
                current.push(c);
                state = InInteger;
            },
            _ => {
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
                assert!(!current.is_empty());
                let previous = current.chars().next().unwrap();
                match previous {
                    '=' => tokens.push(Token::Equals),
                    '!' => tokens.push(Token::Bang),
                    '<' => tokens.push(Token::Less),
                    '>' => tokens.push(Token::Greater),
                    '&' => tokens.push(Token::SingleAnd),
                    '|' => tokens.push(Token::SingleOr),
                    _ => {
                        panic!("unexpected prev char in operator");
                    },
                }
            },
            Slash => {
                tokens.push(Token::Slash);
            },
            LineComment => {
                tokens.push(Token::LineComment(current));
            },
            BlockComment | BlockCommentStar => {
                // Block comment was never closed
                tokens.push(Token::Unknown(current));
            },
            InName => {
                tokens.push(name_token(current));
            },
            InType => {
                tokens.push(Token::TypeName(current));
            },
            InInteger => {
                let n = i64::from_str_radix(current.as_str(), 10);
                tokens.push(Token::IntLiteral(n.unwrap()));
            },
            InString => {
                // Unclosed string
                tokens.push(Token::Unknown(current));
            },
            Unknown => {
                tokens.push(Token::Unknown(current));
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
    StringLiteral(String),

    // Idea: These could contain start/end locations (or just end locations?)
    // and expect callers to find the value in the file
    LineComment(String),
    BlockComment(String),
}

// struct Location {
//     col: u32,
//     line: u32,
// }
