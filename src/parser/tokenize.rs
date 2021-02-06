use super::tokens::Token;
use super::location::Location;

pub fn tokenize(text: &str) -> Vec<(Token, Location)> {
    let mut tokens = vec![];
    let mut current = String::new();
    // Location where the current token started
    let mut start_location = Location::new();
    // Location _after_ the current charater
    let mut next_location = Location::new();

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
        // Location of the current character
        let location = next_location;
        next_location = location.update(c);

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
                            tokens.push((Token::DoubleEquals, start_location));
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push((Token::Equals, start_location));
                        }
                    },
                    '!' => {
                        if c == '=' {
                            tokens.push((Token::NotEquals, start_location));
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push((Token::Bang, start_location));
                        }
                    },
                    '<' => {
                        if c == '=' {
                            tokens.push((Token::LessEquals, start_location));
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push((Token::Less, start_location));
                        }
                    },
                    '>' => {
                        if c == '=' {
                            tokens.push((Token::GreaterEquals, start_location));
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push((Token::Greater, start_location));
                        }
                    },
                    '&' => {
                        if c == '&' {
                            tokens.push((Token::DoubleAnd, start_location));
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push((Token::SingleAnd, start_location));
                        }
                    },
                    '|' => {
                        if c == '|' {
                            tokens.push((Token::DoubleOr, start_location));
                            current = String::new();
                            state = Start;
                            continue;
                        } else {
                            tokens.push((Token::SingleOr, start_location));
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
                    tokens.push((Token::Slash, start_location));
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
                    tokens.push((Token::BlockComment(current), start_location));
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
                    tokens.push((Token::LineComment(current), start_location));
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
                tokens.push((name_token(current), start_location));
            },
            InType => {
                debug_assert!(!current.is_empty());
                if c.is_alphanumeric() || c == '_' {
                    current.push(c);
                    continue;
                }
                tokens.push((Token::TypeName(current), start_location));
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
                tokens.push((Token::IntLiteral(n.unwrap()), start_location));
            },
            InFloat => {
                debug_assert!(!current.is_empty());
                if c.is_ascii_digit() {
                    current.push(c);
                    continue;
                }
                // TODO: Handle exponents
                let n = current.parse();
                tokens.push((Token::FloatLiteral(n.unwrap()), start_location));
            },
            InString => {
                debug_assert!(!current.is_empty());
                current.push(c);
                if c == '"' {
                    tokens.push((Token::StringLiteral(current), start_location));
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
                continue;
            },
            Unknown => {
                debug_assert!(!current.is_empty());
                if !(c.is_whitespace() || c == '(' || c == ')'
                     || c == '[' || c == ']' || c == '{' || c == '}') {
                    current.push(c);
                    continue;
                }
                tokens.push((Token::Unknown(current), start_location));
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
                start_location = location;
                state = Slash;
            },
            '%' => tokens.push((Token::Percent, location)),
            '~' => tokens.push((Token::Tilda, location)),
            '^' => tokens.push((Token::Caret, location)),
            '=' | '!' | '>' | '<' | '&' | '|' => {
                current.push(c);
                start_location = location;
                state = Operator;
            },
            '"' => {
                current.push(c);
                start_location = location;
                state = InString;
            },
            _ if c.is_alphabetic() || c == '_' => {
                start_location = location;
                current.push(c);
                if c.is_uppercase() {
                    state = InType;
                } else {
                    state = InName;
                }
            },
            _ if c.is_ascii_digit() => {
                start_location = location;
                current.push(c);
                state = InInteger;
            },
            _ => {
                start_location = location;
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
                    '=' => tokens.push((Token::Equals, start_location)),
                    '!' => tokens.push((Token::Bang, start_location)),
                    '<' => tokens.push((Token::Less, start_location)),
                    '>' => tokens.push((Token::Greater, start_location)),
                    '&' => tokens.push((Token::SingleAnd, start_location)),
                    '|' => tokens.push((Token::SingleOr, start_location)),
                    _ => {
                        panic!("unexpected prev char in operator");
                    },
                }
            },
            Slash => {
                tokens.push((Token::Slash, start_location));
            },
            LineComment => {
                tokens.push((Token::LineComment(current), start_location));
            },
            BlockComment | BlockCommentStar => {
                // Block comment was never closed
                tokens.push((Token::Unknown(current), start_location));
            },
            InName => {
                tokens.push((name_token(current), start_location));
            },
            InType => {
                tokens.push((Token::TypeName(current), start_location));
            },
            InInteger => {
                let n = current.parse();
                tokens.push((Token::IntLiteral(n.unwrap()), start_location));
            },
            InFloat => {
                let n = current.parse();
                tokens.push((Token::FloatLiteral(n.unwrap()), start_location));
            },
            InString | InStringEscape => {
                // Unclosed string
                tokens.push((Token::Unknown(current), start_location));
            },
            Unknown => {
                tokens.push((Token::Unknown(current), start_location));
            },
        }
    }

    tokens
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

fn untokenize(tokens: Vec<(Token, Location)>) -> String {
    let mut result = String::new();
    let mut here = Location::new();

    for (token, location) in tokens {
        if location.is_before(here) {
            panic!("can't rewind, currently have {}", result);
        }

        // Pad the input with requested whitespace.
        // Newlines are literal tokens, so it shouldn't be necessary to pad them.
        if here.line != location.line {
            panic!("lost a newline");
        }
        while here.col < location.col {
            result.push(' ');
            here = here.update(' ');
        }

        let formatted = format!("{}", token);
        for c in formatted.chars() {
            here = here.update(c);
        }

        result.push_str(&formatted);
    }

    return result;
}

#[cfg(test)]
mod test {
    use super::*;
    use Token::*;

    fn get_tokens(input: &str) -> Vec<Token> {
        tokenize(input).iter().map(|(token, _)| { token }).cloned().collect()
    }

    fn assert_is_token(expected: Token, input: &str) {
        assert_eq!(vec![expected], get_tokens(input));
    }

    fn assert_tokens(expected: Vec<Token>, input: &str) {
        assert_eq!(expected, get_tokens(input));
    }

    #[test]
    fn test_tokenize_empty() {
        let empty: Vec<(Token, Location)> = vec![];
        assert_eq!(empty, tokenize(""));
    }

    #[test]
    fn test_words() {
        assert_is_token(ValueName("foo".to_string()), "foo");
        assert_is_token(ValueName("x".to_string()), "x");
        assert_is_token(ValueName("b_A_r2".to_string()), "b_A_r2");
        assert_is_token(TypeName("Int".to_string()), "Int");
        assert_is_token(TypeName("X".to_string()), "X");
        assert_is_token(KeywordFn, "fn");
    }

    #[test]
    fn test_strings() {
        assert_is_token(StringLiteral(r#""""#.to_string()), r#""""#);
        assert_is_token(StringLiteral(r#""foo""#.to_string()), r#""foo""#);
        assert_is_token(StringLiteral(r#""foo\n""#.to_string()), r#""foo\n""#);
        assert_is_token(StringLiteral(r#""foo\"""#.to_string()), r#""foo\"""#);
    }

    #[test]
    fn test_numbers() {
        assert_is_token(IntLiteral(0), "0");
        assert_is_token(IntLiteral(123), "123");
        assert_is_token(FloatLiteral(123.), "123.");
        assert_is_token(FloatLiteral(3.14159), "3.14159");
    }

    #[test]
    fn test_comments() {
        assert_tokens(
            vec![IntLiteral(123), LineComment(" foo".to_string())],
            "123   // foo"
        );
        assert_tokens(
            vec![IntLiteral(123), LineComment(" foo".to_string()), Newline],
            "123   // foo\n"
        );
        assert_tokens(
            vec![ValueName("x".to_string()), BlockComment("\n comment ".to_string()),
                 Plus, IntLiteral(123)],
            "x  /*\n comment */ +  123"
        );
    }

    #[test]
    fn test_characters() {
        let expected: Vec<Token> = vec![
            DoubleEquals, Equals, LParen, Plus,
            SingleOr, GreaterEquals, Greater,
            DoubleOr, Caret,
        ];
        let input = "===(+|>=>||^";
        assert_tokens(expected, input);
    }

    #[test]
    fn test_unclosed_things() {
        let unclosed_string = vec![(Unknown("\"foo".to_string()), Location::new())];
        assert_eq!(unclosed_string, tokenize("\"foo"));

        let unclosed_escape = vec![(Unknown("\"foo\\".to_string()), Location::new())];
        assert_eq!(unclosed_escape, tokenize("\"foo\\"));

        let unclosed_block_comment = vec![(Unknown("  comment  ".to_string()), Location::new())];
        assert_eq!(unclosed_block_comment, tokenize("/*  comment  *"));
        assert_eq!(unclosed_block_comment, tokenize("/*  comment  "));
    }

    fn assert_untokenizes(input: &str) {
        let result = untokenize(tokenize(input));
        assert_eq!(input, result.as_str());
    }

    #[test]
    fn test_untokenization() {
        // This is actually mainly testing that locations are recorded correctly

        assert_untokenizes("");
        assert_untokenizes("123");
        assert_untokenizes("     123");
        assert_untokenizes(" \n\n    123");
        assert_untokenizes("===(+|>=>||^");
        assert_untokenizes("x  /*\n comment */ +  123");
        assert_untokenizes("type Foo enum:\n  Bar\n  Baz // comment\n")
    }
}
