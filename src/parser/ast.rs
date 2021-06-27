#[cfg(test)]
use std::fmt;

// Index types
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct DeclarationRef(usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct StatementRef(usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ExpressionRef(usize);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct TypeRef(usize);

#[cfg(test)]
pub fn inspect<T>(value: T, syntax: &Syntax) -> Result<String, fmt::Error>
where
    T: Inspect,
{
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
impl Inspect for DeclarationRef {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        s.get_declaration(*self).inspect(f, s)
    }
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

// Syntax contains the results of parsing one file
#[derive(Debug)]
pub struct Syntax {
    pub filename: String,

    pub declarations: Vec<Declaration>,
    pub statements: Vec<Statement>,
    pub expressions: Vec<Expression>,
    pub types: Vec<Type>,

    pub errors: Vec<String>,
}

impl Syntax {
    pub fn new(filename: String) -> Self {
        Syntax {
            filename,
            declarations: Vec::new(),
            statements: Vec::new(),
            expressions: Vec::new(),
            types: Vec::new(),
            errors: Vec::new(),
        }
    }

    #[cfg(test)]
    pub fn get_expression(&self, ExpressionRef(r): ExpressionRef) -> &Expression {
        &self.expressions[r]
    }

    #[cfg(test)]
    pub fn get_statement(&self, StatementRef(r): StatementRef) -> &Statement {
        &self.statements[r]
    }

    #[cfg(test)]
    pub fn get_declaration(&self, DeclarationRef(r): DeclarationRef) -> &Declaration {
        &self.declarations[r]
    }

    pub fn get_type(&self, TypeRef(r): TypeRef) -> &Type {
        &self.types[r]
    }

    pub fn add_error(&mut self, message: String) {
        self.errors.push(message);
    }

    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn add_expression(&mut self, expr: Expression) -> ExpressionRef {
        let eref = self.expressions.len();
        self.expressions.push(expr);
        ExpressionRef(eref)
    }

    pub fn add_statement(&mut self, stmt: Statement) -> StatementRef {
        let sref = self.statements.len();
        self.statements.push(stmt);
        StatementRef(sref)
    }

    pub fn add_declaration(&mut self, decl: Declaration) -> DeclarationRef {
        let dref = self.declarations.len();
        self.declarations.push(decl);
        DeclarationRef(dref)
    }

    pub fn add_type(&mut self, t: Type) -> TypeRef {
        let tref = self.types.len();
        self.types.push(t);
        TypeRef(tref)
    }
}

#[derive(Debug)]
pub enum Declaration {
    DeclarationParseError,

    PackageDecl(String),
    ImportDecl(Vec<String>),
    TypeDecl(TypeRef, Box<TypeDefinition>),
    FunctionDecl(Box<FunctionDecl>),
    InstanceDecl(Box<InstanceDecl>),
}

#[cfg(test)]
impl Inspect for &Declaration {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        use Declaration::*;
        match self {
            DeclarationParseError => write!(f, "(decl-error)"),
            PackageDecl(name) => write!(f, "(package {})", name),
            ImportDecl(names) => {
                write!(f, "(import")?;
                for name in names.iter() {
                    write!(f, " {}", name)?;
                }
                write!(f, ")")
            }
            TypeDecl(defined, tdef) => {
                write!(f, "(type ")?;
                defined.inspect(f, s)?;
                write!(f, " ")?;
                tdef.inspect(f, s)?;
                write!(f, ")")
            }
            FunctionDecl(fdecl) => fdecl.inspect(f, s),
            InstanceDecl(idecl) => idecl.inspect(f, s),
        }
    }
}

#[derive(Debug)]
pub enum Statement {
    StatementParseError,

    Return,
    ReturnExpr(ExpressionRef),
    ExprStmt(ExpressionRef),
    LetStmt(Box<LetStatement>),
    AssignStmt(ExpressionRef, ExpressionRef),
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
            StatementParseError => write!(f, "(stmt-error)"),
            Return => write!(f, "(return)"),
            ReturnExpr(expr) => {
                write!(f, "(return ")?;
                expr.inspect(f, s)?;
                write!(f, ")")
            }
            ExprStmt(expr) => {
                write!(f, "(expr ")?;
                expr.inspect(f, s)?;
                write!(f, ")")
            }
            LetStmt(let_stmt) => let_stmt.inspect(f, s),
            AssignStmt(asignee, expr) => {
                write!(f, "(assign ")?;
                asignee.inspect(f, s)?;
                write!(f, " ")?;
                expr.inspect(f, s)?;
                write!(f, ")")
            }
            Block(stmts) => {
                write!(f, "(do")?;
                for stmt in stmts {
                    write!(f, " ")?;
                    stmt.inspect(f, s)?;
                }
                write!(f, ")")
            }
            IfStmt(ifstmt) => ifstmt.inspect(f, s),
            WhileStmt(whilestmt) => whilestmt.inspect(f, s),
            ForStmt(forstmt) => forstmt.inspect(f, s),
            MatchStmt(matchstmt) => matchstmt.inspect(f, s),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub variable: String,
    pub expl_type: Option<TypeRef>,
    pub value: ExpressionRef,
}

#[cfg(test)]
impl Inspect for LetStatement {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(let {} ", self.variable)?;
        if let Some(t) = self.expl_type {
            write!(f, ":: ")?;
            t.inspect(f, s)?;
            write!(f, " ")?;
        }
        self.value.inspect(f, s)?;
        write!(f, ")")
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
    FieldAccess(ExpressionRef, String),         // like 'foo.bar'
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
            }
            BinaryOperator(bop, left, right) => {
                write!(f, "(binary ")?;
                bop.inspect(f, s)?;
                write!(f, " ")?;
                left.inspect(f, s)?;
                write!(f, " ")?;
                right.inspect(f, s)?;
                write!(f, ")")
            }
            FunctionCall(fexpr, ref args) => {
                write!(f, "(call ")?;
                fexpr.inspect(f, s)?;
                for arg in args {
                    write!(f, " ")?;
                    arg.inspect(f, s)?;
                }
                write!(f, ")")
            }
            FieldAccess(expr, ref field) => {
                write!(f, "(access ")?;
                expr.inspect(f, s)?;
                write!(f, " {})", field)
            }
            OffsetAccess(expr, offset) => {
                write!(f, "(offset ")?;
                expr.inspect(f, s)?;
                write!(f, " ")?;
                offset.inspect(f, s)?;
                write!(f, ")")
            }
            Paren(expr) => {
                write!(f, "(paren ")?;
                expr.inspect(f, s)?;
                write!(f, ")")
            }
            StructCreate(ref structexpr) => structexpr.inspect(f, s),
            Lambda(ref lambda) => lambda.inspect(f, s),
        }
    }
}

#[derive(Debug)]
pub enum Type {
    TypeParseError,

    Void,
    TypeName(String),
    TypeVar(String),
    Generic(String, Vec<TypeRef>),
    FnType(Box<FuncType>),
}

impl Type {
    pub fn declared_name(&self) -> Option<String> {
        use Type::*;
        match self {
            TypeName(s) => Some(s.clone()),
            Generic(s, _) => Some(s.clone()),
            _ => None,
        }
    }
}

#[cfg(test)]
impl Inspect for Type {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        use Type::*;
        match self {
            TypeParseError => write!(f, "type-err"),
            Void => write!(f, "void"),
            TypeName(name) => write!(f, "{}", name),
            TypeVar(name) => write!(f, "(tvar {})", name),
            Generic(name, refs) => {
                write!(f, "(generic {}", name)?;
                for t in refs {
                    write!(f, " ")?;
                    t.inspect(f, s)?;
                }
                write!(f, ")")
            }
            FnType(func_type) => func_type.inspect(f, s),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FuncType {
    pub arg_types: Vec<TypeRef>,
    pub ret_type: TypeRef,
    pub constraints: Vec<Constraint>,
}

#[cfg(test)]
impl Inspect for FuncType {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(function ")?;
        inspect_list(&self.arg_types, f, s)?;
        write!(f, " ")?;
        self.ret_type.inspect(f, s)?;
        if !self.constraints.is_empty() {
            write!(f, " where ")?;
            inspect_list(&self.constraints, f, s)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, PartialEq)]
pub struct Constraint {
    pub constrained: TypeRef,
    pub class: String,
}

#[cfg(test)]
impl Inspect for Constraint {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(")?;
        self.constrained.inspect(f, s)?;
        write!(f, " {})", self.class)
    }
}

#[derive(Debug, PartialEq)]
pub struct StructExpression {
    pub struct_name: String,
    pub fields: Vec<(String, ExpressionRef)>,
}

#[cfg(test)]
impl Inspect for StructExpression {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(make-struct {}", self.struct_name)?;
        for (field_name, expr) in self.fields.iter() {
            write!(f, " ({} ", field_name)?;
            expr.inspect(f, s)?;
            write!(f, ")")?;
        }
        write!(f, ")")
    }
}

#[derive(Debug)]
pub enum TypeDefinition {
    Alias(TypeRef),
    Structure(StructType),
    Enum(EnumType),
    Class(ClassType),
}

#[cfg(test)]
impl Inspect for TypeDefinition {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        use TypeDefinition::*;
        match self {
            Alias(tref) => {
                write!(f, "alias ")?;
                tref.inspect(f, s)
            }
            Structure(struct_type) => struct_type.inspect(f, s),
            Enum(enum_type) => enum_type.inspect(f, s),
            Class(class_type) => class_type.inspect(f, s),
        }
    }
}

#[derive(Debug)]
pub struct StructType {
    pub fields: Vec<(String, TypeRef)>,
}

#[cfg(test)]
impl Inspect for StructType {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(struct")?;
        self.write_inner(f, s)?;
        write!(f, ")")
    }
}

#[cfg(test)]
impl StructType {
    fn write_inner(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        for (name, t) in self.fields.iter() {
            write!(f, " ({} ", name)?;
            t.inspect(f, s)?;
            write!(f, ")")?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct EnumType {
    pub variants: Vec<EnumVariant>,
}

#[cfg(test)]
impl Inspect for EnumType {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(enum")?;
        for variant in self.variants.iter() {
            write!(f, " ")?;
            variant.inspect(f, s)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug)]
pub struct EnumVariant {
    pub name: String,
    pub content: StructType,
}

#[cfg(test)]
impl Inspect for EnumVariant {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "({}", self.name)?;
        self.content.write_inner(f, s)?;
        write!(f, ")")
    }
}

#[derive(Debug)]
pub struct ClassType {
    pub super_classes: Vec<String>,
    pub methods: Vec<ClassMethod>,
}

#[cfg(test)]
impl Inspect for ClassType {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(class")?;

        if !self.super_classes.is_empty() {
            write!(f, " extends ")?;
            inspect_list(&self.super_classes, f, s)?;
        }

        for m in self.methods.iter() {
            write!(f, " ")?;
            m.inspect(f, s)?;
        }

        write!(f, ")")
    }
}

#[derive(Debug, PartialEq)]
pub struct ClassMethod {
    pub name: String,
    pub ftype: TypeRef,
}

#[cfg(test)]
impl Inspect for ClassMethod {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "({} :: ", self.name)?;
        self.ftype.inspect(f, s)?;
        write!(f, ")")
    }
}

#[derive(Debug, PartialEq)]
pub struct Lambda {
    pub arg_names: Vec<String>,
    pub body: StatementRef,
}

#[cfg(test)]
impl Inspect for Lambda {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(lambda (")?;
        let mut wrote_first = false;
        for name in self.arg_names.iter() {
            if wrote_first {
                write!(f, " ")?;
            }
            write!(f, "{}", name)?;
            wrote_first = true;
        }
        write!(f, ") ")?;
        self.body.inspect(f, s)?;
        write!(f, ")")
    }
}

#[derive(Debug)]
pub struct IfStatement {
    pub test: ExpressionRef,
    pub tbody: StatementRef,
    pub ebody: Option<StatementRef>,
}

#[cfg(test)]
impl Inspect for IfStatement {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(if ")?;
        self.test.inspect(f, s)?;
        write!(f, " ")?;
        self.tbody.inspect(f, s)?;
        if let Some(stmt) = self.ebody {
            write!(f, " ")?;
            stmt.inspect(f, s)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug)]
pub struct WhileStatement {
    pub test: ExpressionRef,
    pub body: StatementRef,
}

#[cfg(test)]
impl Inspect for WhileStatement {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(while ")?;
        self.test.inspect(f, s)?;
        write!(f, " ")?;
        self.body.inspect(f, s)?;
        write!(f, ")")
    }
}

#[derive(Debug)]
pub struct ForStatement {
    pub variable: String,
    pub iterable: ExpressionRef,
    pub body: StatementRef,
}

#[cfg(test)]
impl Inspect for ForStatement {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(for {} in ", self.variable)?;
        self.iterable.inspect(f, s)?;
        write!(f, " ")?;
        self.body.inspect(f, s)?;
        write!(f, ")")
    }
}

#[derive(Debug)]
pub struct MatchStatement {
    pub matched: ExpressionRef,
    pub matchers: Vec<Matcher>,
}

#[cfg(test)]
impl Inspect for MatchStatement {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(match ")?;
        self.matched.inspect(f, s)?;
        for matcher in self.matchers.iter() {
            write!(f, " ")?;
            matcher.inspect(f, s)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug)]
pub struct Matcher {
    pub pattern: Pattern,
    pub body: StatementRef,
}

#[cfg(test)]
impl Inspect for Matcher {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(case ")?;
        self.pattern.inspect(f, s)?;
        write!(f, " ")?;
        self.body.inspect(f, s)?;
        write!(f, ")")
    }
}

#[derive(Debug)]
pub enum Pattern {
    PatternParseError,

    Underscore,
    Name(String),
    Named(String, Box<Pattern>),
    Literal(Literal),
    Structure(Box<StructPattern>),
    Tuple(Vec<Pattern>),
}

#[cfg(test)]
impl Inspect for Pattern {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        use Pattern::*;
        match self {
            PatternParseError => write!(f, "pattern-err"),
            Underscore => write!(f, "wildcard"),
            Name(name) => write!(f, "{}", name),
            Named(name, pattern) => {
                write!(f, "(@ {} ", name)?;
                pattern.inspect(f, s)?;
                write!(f, ")")
            }
            Literal(lit) => {
                write!(f, "(lit ")?;
                lit.inspect(f, s)?;
                write!(f, ")")
            }
            Structure(pat) => pat.inspect(f, s),
            Tuple(pats) => {
                write!(f, "(tuple")?;
                for pat in pats.iter() {
                    write!(f, " ")?;
                    pat.inspect(f, s)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug)]
pub struct StructPattern {
    pub struct_name: String,
    pub field_patterns: Vec<Pattern>,
}

#[cfg(test)]
impl Inspect for StructPattern {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(match-struct {}", self.struct_name)?;
        for pat in self.field_patterns.iter() {
            write!(f, " ")?;
            pat.inspect(f, s)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug)]
pub struct InstanceDecl {
    pub on_type: TypeRef,
    pub class: String,
    pub constraints: Vec<Constraint>,
    pub methods: Vec<DeclarationRef>,
}

#[cfg(test)]
impl Inspect for InstanceDecl {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(instance {} ", self.class)?;
        self.on_type.inspect(f, s)?;
        if !self.constraints.is_empty() {
            write!(f, " where ")?;
            inspect_list(&self.constraints, f, s)?;
        }
        for method in self.methods.iter() {
            write!(f, " ")?;
            method.inspect(f, s)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug)]
pub struct FunctionDecl {
    pub name: String,
    pub arg_names: Vec<String>,
    pub fn_type: Option<TypeRef>,
    pub body: StatementRef,
}

#[cfg(test)]
impl Inspect for FunctionDecl {
    fn inspect(&self, f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result {
        write!(f, "(defn {} (", self.name)?;
        let mut wrote_first = false;
        for arg in self.arg_names.iter() {
            if wrote_first {
                write!(f, " ")?;
            }
            write!(f, "{}", arg)?;
            wrote_first = true;
        }
        write!(f, ") ")?;
        if let Some(tref) = self.fn_type {
            write!(f, ":: ")?;
            tref.inspect(f, s)?;
            write!(f, " ")?;
        }
        self.body.inspect(f, s)?;
        write!(f, ")")
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
    fn inspect(&self, f: &mut impl fmt::Write, _s: &Syntax) -> fmt::Result {
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
    fn inspect(&self, f: &mut impl fmt::Write, _s: &Syntax) -> fmt::Result {
        use UnaryOp::*;
        match self {
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
    fn inspect(&self, f: &mut impl fmt::Write, _s: &Syntax) -> fmt::Result {
        use BinaryOp::*;
        match self {
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Times => write!(f, "*"),
            Divide => write!(f, "/"),
            Mod => write!(f, "%"),
            Power => write!(f, "**"),
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

#[cfg(test)]
impl Inspect for String {
    fn inspect(&self, f: &mut impl fmt::Write, _s: &Syntax) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[cfg(test)]
fn inspect_list<T>(items: &[T], f: &mut impl fmt::Write, s: &Syntax) -> fmt::Result
where
    T: Inspect,
{
    write!(f, "(")?;
    let mut wrote_first = false;
    for item in items.iter() {
        if wrote_first {
            write!(f, " ")?;
        }
        wrote_first = true;
        item.inspect(f, s)?;
    }
    write!(f, ")")
}
