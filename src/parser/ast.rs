pub type NodeRef = u32;


pub struct Syntax {
    nodes: Vec<Node>,
}

pub enum Node {
    // Types
    Type(String),
    Generic(String, Vec<NodeRef>),
    FnType(Vec<NodeRef>, NodeRef),

    // Expressions
    Literal(Literal),
    Variable(String),
    UnaryOperator(UnaryOp, NodeRef),
    BinaryOperator(BinaryOp, NodeRef, NodeRef),
    FunctionCall(NodeRef, Vec<NodeRef>),
    StructCreate(Box<StructExpression>),
    FieldAccess(NodeRef, NodeRef), // like 'foo.bar'
    OffsetAccess(NodeRef, NodeRef), // like 'foo[bar]'
    Lambda(Box<Lambda>),
    Paren(NodeRef),

    // Statements
    Return,
    ReturnExpr(NodeRef),
    ExprStmt(NodeRef),
    LetStmt(String, NodeRef),
    AssignStmt(String, NodeRef),
    IfStmt(Box<IfStatement>),
    WhileStmt(Box<WhileStatement>),
    ForStmt(Box<ForStatement>),
    MatchStmt(Box<MatchStatement>),
    Block(Vec<NodeRef>),

    // Declarations
    PackageDecl(String),
    ImportDecl(String),
    TypeDecl(String, NodeRef),
    FunctionDecl(Box<FunctionDecl>),

    // Helpers
    StructTypeDecl(Box<StructType>),
    EnumTypeDecl(Box<EnumType>),
}

pub struct StructExpression {
    struct_name: String,
    field_names: Vec<String>,
    expressions: Vec<NodeRef>,
}

pub struct StructType {
    field_names: Vec<String>,
    field_types: Vec<NodeRef>,
}

pub struct EnumType {
    variants: Vec<EnumVariant>,
}

pub struct EnumVariant {
    name: String,
    content: StructType,
}

pub struct Lambda {
    arg_names: Vec<String>,
    body: NodeRef,
}

pub struct IfStatement {
    test: NodeRef,
    tbody: NodeRef,
    ebody: Option<NodeRef>,
}

pub struct WhileStatement {
    test: NodeRef,
    body: NodeRef,
}

pub struct ForStatement {
    variable: String,
    iterable: NodeRef,
    body: NodeRef,
}

pub struct MatchStatement {
    matched: NodeRef,
    matchers: Vec<Matcher>,
}

pub struct Matcher {
    pattern: Pattern,
    body: NodeRef,
}

pub enum Pattern {
    Underscore,
    Name(String),
    Named(String, Box<Pattern>),
    Literal(Literal),
    Structure(NodeRef, Vec<Pattern>),
}

pub struct FunctionDecl {
    name: String,
    arg_names: Vec<String>,
    fn_type: Option<NodeRef>,
    body: NodeRef,
}

pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    BoolNot,
    Negate,
    BitInvert,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOp {
    Plus,
    Minus,
    Times,
    Divide,
    Mod,
    Power,

    BitAnd,
    BitOr,

    BoolAnd,
    BoolOr,

    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    // TODO: left and right shift
}
