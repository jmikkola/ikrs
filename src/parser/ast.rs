pub type StatementRef = usize;
pub type ExpressionRef = usize;
pub type TypeRef = usize;

// Syntax contains the results of parsing one file
#[derive(Debug)]
pub struct Syntax {
    pub declarations: Vec<Declaration>,
    pub statements: Vec<Statement>,
    pub expressions: Vec<Expression>,
    pub types: Vec<Type>,
}

impl Syntax {
    pub fn new() -> Self {
        Syntax{
            declarations: Vec::new(),
            statements: Vec::new(),
            expressions: Vec::new(),
            types: Vec::new(),
        }
    }

    pub fn add_expression(&mut self, expr: Expression) -> ExpressionRef {
        let eref = self.expressions.len() as ExpressionRef;
        self.expressions.push(expr);
        eref
    }

    pub fn expression_equals(&self, eref: ExpressionRef, other: &Self, oref: ExpressionRef) -> bool {
        use Expression::*;
        match (&self.expressions[eref], &other.expressions[oref]) {
            (ExpressionParseError, ExpressionParseError) => true,
            (Literal(l1), Literal(l2)) => l1 == l2,
            (Variable(s1), Variable(s2)) => s1 == s2,
            (UnaryOperator(o1, e1), UnaryOperator(o2, e2)) => {
                o1 == o2 && self.expression_equals(*e1, other, *e2)
            },
            (BinaryOperator(o1, l1, r1), BinaryOperator(o2, l2, r2)) => {
                o1 == o2 &&
                    self.expression_equals(*l1, other, *l2) &&
                    self.expression_equals(*r1, other, *r2)
            },
            (FunctionCall(e1, a1), FunctionCall(e2, a2)) => {
                self.expression_equals(*e1, other, *e2) &&
                    self.exprs_equal(a1, other, a2)
            },
            (FieldAccess(a1, f1), FieldAccess(a2, f2)) => {
                self.expression_equals(*a1, other, *a2) &&
                    f1 == f2
            },
            (OffsetAccess(a1, b1), OffsetAccess(a2, b2)) => {
                self.expression_equals(*a1, other, *a2) &&
                    self.expression_equals(*b1, other, *b2)
            },
            (Paren(a1), Paren(a2)) => self.expression_equals(*a1, other, *a2),
            (StructCreate(se1), StructCreate(se2)) => {
                panic!("TODO");
            },
            (Lambda(l1), Lambda(l2)) => {
                panic!("TODO");
            },

            _ => false,
        }
    }

    fn exprs_equal(&self, erefs: &Vec<ExpressionRef>, other: &Self, orefs: &Vec<ExpressionRef>) -> bool {
        if erefs.len() != orefs.len() {
            return false
        }
        erefs.iter().zip(orefs)
            .all(|(e, o)| self.expression_equals(*e, other, *o))
    }
}

#[derive(Debug)]
pub enum Declaration {
    DeclarationParseError,

    PackageDecl(String),
    ImportDecl(String),
    TypeDecl(String, Box<TypeDefinition>),
    FunctionDecl(Box<FunctionDecl>),
}

#[derive(Debug)]
pub enum Statement {
    StatementParseError,

    Return,
    ReturnExpr(ExpressionRef),
    ExprStmt(ExpressionRef),
    LetStmt(String, ExpressionRef),
    AssignStmt(String, ExpressionRef),
    Block(Vec<StatementRef>),

    IfStmt(Box<IfStatement>),
    WhileStmt(Box<WhileStatement>),
    ForStmt(Box<ForStatement>),
    MatchStmt(Box<MatchStatement>),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    ExpressionParseError,

    Literal(Literal),
    Variable(String),
    UnaryOperator(UnaryOp, ExpressionRef),
    BinaryOperator(BinaryOp, ExpressionRef, ExpressionRef),
    FunctionCall(ExpressionRef, Vec<ExpressionRef>),
    FieldAccess(ExpressionRef, String), // like 'foo.bar'
    OffsetAccess(ExpressionRef, ExpressionRef), // like 'foo[bar]'
    Paren(ExpressionRef),

    StructCreate(Box<StructExpression>),
    Lambda(Box<Lambda>),
}

#[derive(Debug)]
pub enum Type {
    TypeParseError,

    TypeName(String),
    Generic(String, Vec<TypeRef>),
    FnType(Vec<TypeRef>, TypeRef),
}

#[derive(Debug, PartialEq)]
pub struct StructExpression {
    struct_name: String,
    field_names: Vec<String>,
    expressions: Vec<ExpressionRef>,
}

#[derive(Debug)]
pub enum TypeDefinition {
    Alias(TypeRef),
    Structure(StructType),
    Enum(EnumType),
}

#[derive(Debug)]
pub struct StructType {
    field_names: Vec<String>,
    field_types: Vec<TypeRef>, // Can only refer to types, can't define new ones
}

#[derive(Debug)]
pub struct EnumType {
    variants: Vec<EnumVariant>,
}

#[derive(Debug)]
pub struct EnumVariant {
    name: String,
    content: StructType,
}

#[derive(Debug, PartialEq)]
pub struct Lambda {
    arg_names: Vec<String>,
    body: ExpressionRef,
}

#[derive(Debug)]
pub struct IfStatement {
    test: ExpressionRef,
    tbody: StatementRef,
    ebody: Option<StatementRef>,
}

#[derive(Debug)]
pub struct WhileStatement {
    test: ExpressionRef,
    body: StatementRef,
}

#[derive(Debug)]
pub struct ForStatement {
    variable: String,
    iterable: ExpressionRef,
    body: StatementRef,
}

#[derive(Debug)]
pub struct MatchStatement {
    matched: ExpressionRef,
    matchers: Vec<Matcher>,
}

#[derive(Debug)]
pub struct Matcher {
    pattern: Pattern,
    body: StatementRef,
}

#[derive(Debug)]
pub enum Pattern {
    Underscore,
    Name(String),
    Named(String, Box<Pattern>),
    Literal(Literal),
    Structure(Box<StructPattern>, Vec<Pattern>),
}

#[derive(Debug)]
pub struct StructPattern {
    names: Vec<String>,
    patterns: Vec<Pattern>,
}

#[derive(Debug)]
pub struct FunctionDecl {
    name: String,
    arg_names: Vec<String>,
    fn_type: Option<TypeRef>,
    body: StatementRef,
}

#[derive(Debug, PartialEq)]
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
