#[derive(Debug)]
pub enum Token {
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
