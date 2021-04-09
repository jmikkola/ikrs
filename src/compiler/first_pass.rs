use std::collections::HashSet;
use std::io;

use super::super::parser::ast;

// This performs several checks prior to type inference:
// - Declarations in a module declare unique names
// - Functions do not declare duplicate variables or arguments
// - Structure, class, and implementation declarations are well-formed
// - Multiple class definitions do not use overlapping method names
// - The class hierarchy is acyclic
pub fn check(syntax: &ast::Syntax) -> io::Result<()> {
    let mut state = CheckState::new(syntax);
    state.check_syntax();
    state.print_result()
}

struct CheckState<'a> {
    syntax: &'a ast::Syntax,
    errors: Vec<String>,

    binding_names: HashSet<String>,
    package_decl_set: bool,
}

impl<'a> CheckState<'a> {
    fn new(syntax: &'a ast::Syntax) -> Self {
        CheckState{
            syntax: syntax,
            errors: Vec::new(),

            binding_names: HashSet::new(),
            package_decl_set: false,
        }
    }

    fn print_result(&self) -> io::Result<()> {
        if self.errors.is_empty() {
            return Ok(());
        }

        for error in self.errors.iter() {
            println!("{}", error);
        }

        let err = io::Error::new(io::ErrorKind::Other, "checks failed");
        Err(err)
    }

    fn check_syntax(&mut self) {
        for declaration in self.syntax.declarations.iter() {
            self.check_declaration(declaration);
        }
        self.check_class_hierarchy();
    }

    fn check_declaration(&mut self, declaration: &ast::Declaration) {
        use ast::Declaration::*;

        match declaration {
            DeclarationParseError => {
                panic!("should not see a parse error in first pass")
            },
            PackageDecl(name) => {
                if self.package_decl_set {
                    self.add_error("duplicate package declaration");
                }
                self.package_decl_set = true;
            },
            ImportDecl(names) => {
                // TODO: Assert that imports are not duplicated
            },
            TypeDecl(tdecl, tdef) => {
                // TODO: Check that the type defined is not duplicated
                // TODO: Check the validity of the defined type (references
                // defined types, no duplicate fields, etc)
                // TODO: If the type is a class, record information for checking
                // the class hierarchy and check method definition soundness
            },
            FunctionDecl(func_decl) => {
                // TODO: Check arguments
                // TODO: Check function body
            },
            InstanceDecl(inst_decl) => {
                // TODO: Check that it actually implements the class it says it does
            },
        }
    }

    fn check_class_hierarchy(&mut self) {
        // TODO
    }

    fn add_error(&mut self, err: &str) {
        self.errors.push(err.to_owned());
    }
}
