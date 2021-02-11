use std::fmt;

// Syntax contains the results of parsing one file
#[derive(Debug)]
pub struct Syntax {
    pub declarations: Vec<Declaration>,
    pub statements: Vec<Statement>,
    pub expressions: Vec<Expression>,
    pub types: Vec<Type>,
}

// Index types
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StatementRef(usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ExpressionRef(usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TypeRef(usize);

#[cfg(test)]
pub fn inspect<T>(value: T, syntax: &Syntax) -> Result<String, fmt::Error>
where T: Inspect {
    let mut result = String::new();
    value.inspect(&mut result, syntax)?;
    Ok(result)
}

// Inspection trait for debugging the parser
#[cfg(test)]
pub trait Inspect {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result;
}

#[cfg(test)]
impl Inspect for ExpressionRef {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        s.get_expression(*self).inspect(f, s)
    }
}

#[cfg(test)]
impl Inspect for StatementRef {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        s.get_statement(*self).inspect(f, s)
    }
}

#[cfg(test)]
impl Inspect for TypeRef {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        s.get_type(*self).inspect(f, s)
    }
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

    pub fn get_expression(&self, ExpressionRef(r): ExpressionRef) -> &Expression {
        &self.expressions[r]
    }

    pub fn get_statement(&self, StatementRef(r): StatementRef) -> &Statement {
        &self.statements[r]
    }

    pub fn get_type(&self, TypeRef(r): TypeRef) -> &Type {
        &self.types[r]
    }

    pub fn add_expression(&mut self, expr: Expression) -> ExpressionRef {
        let eref = self.expressions.len();
        self.expressions.push(expr);
        ExpressionRef(eref)
    }

    pub fn expression_equals(&self, ExpressionRef(eref): ExpressionRef, other: &Self, ExpressionRef(oref): ExpressionRef) -> bool {
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

#[cfg(test)]
impl Inspect for Declaration {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        use Declaration::*;
        match self {
            DeclarationParseError => write!(f, "(decl-error)"),
            PackageDecl(name) => write!(f, "(package {})", name),
            ImportDecl(name) => write!(f, "(import {})", name),
            TypeDecl(name, tdef) => {
                write!(f, "(type {} ", name)?;
                tdef.inspect(f, s)?;
                write!(f, ")")
            },
            FunctionDecl(fdecl) => fdecl.inspect(f, s),
        }
    }
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

#[cfg(test)]
impl Inspect for Statement {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        use Statement::*;
        match self {
            StatementParseError =>
                write!(f, "(stmt-error)"),
            Return =>
                write!(f, "(return)"),
            ReturnExpr(expr) => {
                write!(f, "(return ")?;
                expr.inspect(f, s)?;
                write!(f, ")")
            },
            ExprStmt(expr) => expr.inspect(f, s),
            LetStmt(var, expr) => {
                write!(f, "(let {} ",  var)?;
                expr.inspect(f, s)?;
                write!(f, ")")
            },
            AssignStmt(var, expr) => {
                write!(f, "(assign {} ",  var)?;
                expr.inspect(f, s)?;
                write!(f, ")")
            },
            Block(stmts) => {
                write!(f, "(do")?;
                for stmt in stmts {
                    write!(f, " ")?;
                    stmt.inspect(f, s)?;
                }
                write!(f, ")")
            },
            IfStmt(ifstmt) => ifstmt.inspect(f, s),
            WhileStmt(whilestmt) => whilestmt.inspect(f, s),
            ForStmt(forstmt) => forstmt.inspect(f, s),
            MatchStmt(matchstmt) => matchstmt.inspect(f, s),
        }
    }
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


#[cfg(test)]
impl Inspect for Expression {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        use Expression::*;
        match &*self {
            ExpressionParseError => write!(f, "expr-error"),
            Literal(l) => l.inspect(f, s),
            Variable(ref s) => write!(f, "{}", s),
            UnaryOperator(uop, expr) => {
                write!(f, "(unary ")?;
                uop.inspect(f, s)?;
                write!(f, " ")?;
                expr.inspect(f, s)?;
                write!(f, ")")
            },
            BinaryOperator(bop, left, right) => {
                write!(f, "(binary ")?;
                bop.inspect(f, s)?;
                write!(f, " ")?;
                left.inspect(f, s)?;
                write!(f, " ")?;
                right.inspect(f, s)?;
                write!(f, ")")
            },
            FunctionCall(fexpr, ref args) => {
                write!(f, "(call ")?;
                fexpr.inspect(f, s)?;
                for arg in args {
                    write!(f, " ")?;
                    arg.inspect(f, s)?;
                }
                write!(f, ")")
            },
            FieldAccess(expr, ref field) => {
                write!(f, "(access ")?;
                expr.inspect(f, s)?;
                write!(f, " {})", field)
            },
            OffsetAccess(expr, offset) => {
                write!(f, "(offset ")?;
                expr.inspect(f, s)?;
                write!(f, " ")?;
                offset.inspect(f, s)?;
                write!(f, ")")
            },
            Paren(expr) => {
                write!(f, "(paren ")?;
                expr.inspect(f, s)?;
                write!(f, ")")
            },
            StructCreate(ref structexpr) => structexpr.inspect(f, s),
            Lambda(ref lambda) => lambda.inspect(f, s),
        }
    }
}

#[derive(Debug)]
pub enum Type {
    TypeParseError,

    TypeName(String),
    Generic(String, Vec<TypeRef>),
    FnType(Vec<TypeRef>, TypeRef),
}

#[cfg(test)]
impl Inspect for Type {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        use Type::*;
        match self {
            TypeParseError => write!(f, "type-err"),
            TypeName(name) => write!(f, "{}", name),
            Generic(name, refs) => {
                write!(f, "(generic {}", name)?;
                for t in refs {
                    write!(f, " ")?;
                    t.inspect(f, s)?;
                }
                write!(f, ")")
            },
            FnType(arg_types, ret_type) => {
                write!(f, "(function (")?;
                for t in arg_types {
                    write!(f, " ")?;
                    t.inspect(f, s)?;
                }
                write!(f, ") ")?;
                ret_type.inspect(f, s)?;
                write!(f, ")")
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StructExpression {
    struct_name: String,
    field_names: Vec<String>,
    expressions: Vec<ExpressionRef>,
}

#[cfg(test)]
impl Inspect for StructExpression {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        Ok(()) // TODO
    }
}

#[derive(Debug)]
pub enum TypeDefinition {
    Alias(TypeRef),
    Structure(StructType),
    Enum(EnumType),
}

#[cfg(test)]
impl Inspect for TypeDefinition {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        // TODO
        Ok(())
    }
}

#[derive(Debug)]
pub struct StructType {
    field_names: Vec<String>,
    field_types: Vec<TypeRef>, // Can only refer to types, can't define new ones
}

#[cfg(test)]
impl Inspect for StructType {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        // TODO
        Ok(())
    }
}

#[derive(Debug)]
pub struct EnumType {
    variants: Vec<EnumVariant>,
}

#[cfg(test)]
impl Inspect for EnumType {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        // TODO
        Ok(())
    }
}

#[derive(Debug)]
pub struct EnumVariant {
    name: String,
    content: StructType,
}

#[cfg(test)]
impl Inspect for EnumVariant {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        // TODO
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub struct Lambda {
    arg_names: Vec<String>,
    body: ExpressionRef,
}

#[cfg(test)]
impl Inspect for Lambda {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        // TODO
        Ok(())
    }
}

#[derive(Debug)]
pub struct IfStatement {
    test: ExpressionRef,
    tbody: StatementRef,
    ebody: Option<StatementRef>,
}

#[cfg(test)]
impl Inspect for IfStatement {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        // TODO
        Ok(())
    }
}

#[derive(Debug)]
pub struct WhileStatement {
    test: ExpressionRef,
    body: StatementRef,
}

#[cfg(test)]
impl Inspect for WhileStatement {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        // TODO
        Ok(())
    }
}

#[derive(Debug)]
pub struct ForStatement {
    variable: String,
    iterable: ExpressionRef,
    body: StatementRef,
}

#[cfg(test)]
impl Inspect for ForStatement {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        // TODO
        Ok(())
    }
}

#[derive(Debug)]
pub struct MatchStatement {
    matched: ExpressionRef,
    matchers: Vec<Matcher>,
}

#[cfg(test)]
impl Inspect for MatchStatement {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        // TODO
        Ok(())
    }
}

#[derive(Debug)]
pub struct Matcher {
    pattern: Pattern,
    body: StatementRef,
}

#[cfg(test)]
impl Inspect for Matcher {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        // TODO
        Ok(())
    }
}

#[derive(Debug)]
pub enum Pattern {
    Underscore,
    Name(String),
    Named(String, Box<Pattern>),
    Literal(Literal),
    Structure(Box<StructPattern>, Vec<Pattern>),
}

#[cfg(test)]
impl Inspect for Pattern {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        // TODO
        Ok(())
    }
}

#[derive(Debug)]
pub struct StructPattern {
    names: Vec<String>,
    patterns: Vec<Pattern>,
}

#[cfg(test)]
impl Inspect for StructPattern {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        // TODO
        Ok(())
    }
}

#[derive(Debug)]
pub struct FunctionDecl {
    name: String,
    arg_names: Vec<String>,
    fn_type: Option<TypeRef>,
    body: StatementRef,
}

#[cfg(test)]
impl Inspect for FunctionDecl {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        // TODO
        Ok(())
    }
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
}

#[cfg(test)]
impl Inspect for Literal {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        use Literal::*;
        match self {
            Integer(n) => write!(f, "{}", n),
            Float(n) => write!(f, "{}", n),
            String(st) => write!(f, "{}", st),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    BoolNot,
    Negate,
    BitInvert,
}

#[cfg(test)]
impl Inspect for UnaryOp {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        use UnaryOp::*;
        match self{
            BoolNot => write!(f, "!"),
            Negate => write!(f, "-"),
            BitInvert => write!(f, "~"),
        }
    }
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

#[cfg(test)]
impl Inspect for BinaryOp {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        use BinaryOp::*;
        match self {
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Times => write!(f, "*"),
            Divide => write!(f, "/"),
            Mod => write!(f, "%"),
            Power => write!(f, "^"),
            BitAnd => write!(f, "&"),
            BitOr => write!(f, "|"),
            BoolAnd => write!(f, "&&"),
            BoolOr => write!(f, "||"),
            Equal => write!(f, "=="),
            NotEqual => write!(f, "!="),
            Less => write!(f, "<"),
            LessEqual => write!(f, "<="),
            Greater => write!(f, ">"),
            GreaterEqual => write!(f, ">="),
        }
    }
}
