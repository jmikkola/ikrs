use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    // Bad tokens:
    Unknown(String),

    Colon,
    Comma,
    Dot,
    Newline,

    Ampersand,
    Bang,
    Caret,
    DoubleAnd,
    DoubleEquals,
    DoubleOr,
    DoubleStar,
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
    KeywordExtends,
    KeywordFn,
    KeywordFor,
    KeywordIf,
    KeywordImport,
    KeywordIn,
    KeywordInstance,
    KeywordLet,
    KeywordMatch,
    KeywordPackage,
    KeywordReturn,
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
}

#[derive(Debug, PartialEq, Clone)]
pub enum Comment {
    // Idea: These could contain start/end locations (or just end locations?)
    // and expect callers to find the value in the file
    LineComment(String),
    BlockComment(String),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            Unknown(s) => write!(f, "{}", s),
            Colon => write!(f, ":"),
            Comma => write!(f, ","),
            Dot => write!(f, "."),
            Newline => write!(f, "\n"),
            Ampersand => write!(f, "@"),
            Bang => write!(f, "!"),
            Caret => write!(f, "^"),
            DoubleAnd => write!(f, "&&"),
            DoubleEquals => write!(f, "=="),
            DoubleOr => write!(f, "||"),
            DoubleStar => write!(f, "**"),
            Equals => write!(f, "="),
            Greater => write!(f, ">"),
            GreaterEquals => write!(f, ">="),
            Less => write!(f, "<"),
            LessEquals => write!(f, "<="),
            Minus => write!(f, "-"),
            NotEquals => write!(f, "!="),
            Percent => write!(f, "%"),
            Plus => write!(f, "+"),
            SingleAnd => write!(f, "&"),
            SingleOr => write!(f, "|"),
            Slash => write!(f, "/"),
            Star => write!(f, "*"),
            Tilda => write!(f, "~"),
            LBrace => write!(f, "{{"),
            LBracket => write!(f, "["),
            LParen => write!(f, "("),
            RBrace => write!(f, "}}"),
            RBracket => write!(f, "]"),
            RParen => write!(f, ")"),
            KeywordClass => write!(f, "class"),
            KeywordElse => write!(f, "else"),
            KeywordEnum => write!(f, "enum"),
            KeywordExtends => write!(f, "extends"),
            KeywordFn => write!(f, "fn"),
            KeywordFor => write!(f, "for"),
            KeywordIf => write!(f, "if"),
            KeywordImport => write!(f, "import"),
            KeywordIn => write!(f, "in"),
            KeywordInstance => write!(f, "instance"),
            KeywordLet => write!(f, "let"),
            KeywordMatch => write!(f, "match"),
            KeywordPackage => write!(f, "package"),
            KeywordReturn => write!(f, "return"),
            KeywordStruct => write!(f, "struct"),
            KeywordType => write!(f, "type"),
            KeywordWhere => write!(f, "where"),
            KeywordWhile => write!(f, "while"),
            KeywordWith => write!(f, "with"),
            ValueName(s) => write!(f, "{}", s),
            TypeName(s) => write!(f, "{}", s),
            IntLiteral(i) => write!(f, "{}", i),
            FloatLiteral(n) => write!(f, "{}", n),
            StringLiteral(s) => write!(f, "{}", s),
        }
    }
}

impl fmt::Display for Comment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Comment::*;
        match self {
            LineComment(s) => write!(f, "//{}", s),
            BlockComment(s) => write!(f, "/*{}*/", s),
        }
    }
}
