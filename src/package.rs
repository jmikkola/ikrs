// mod package
use super::parser::ast;

pub struct ParsedPackage  {
    pub package_name: String,
    pub files: Vec<ParsedFile>,
}

pub struct ParsedFile {
    // pub name: String,
    pub syntax: ast::Syntax,
}

impl ParsedFile {
    pub fn filename(&self) -> String {
        self.syntax.filename.clone()
    }
}

pub struct TypedPackage {
    // TODO
}
