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

    // binding_names: HashSet<String>,
    imports_used: HashSet<String>,
    types_declared: HashSet<String>,

    // Used to keep track of advancement through the allowed ordering of declarations
    // (package?, import*, [function|type]*)
    package_decl_set: bool,
    saw_first_decl: bool,
    saw_non_header_decl: bool,
}

impl<'a> CheckState<'a> {
    fn new(syntax: &'a ast::Syntax) -> Self {
        CheckState{
            syntax: syntax,
            errors: Vec::new(),

            // binding_names: HashSet::new(),
            imports_used: HashSet::new(),
            types_declared: HashSet::new(),

            package_decl_set: false,
            saw_first_decl: false,
            saw_non_header_decl: false,
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
            self.saw_first_decl = true;
        }
        self.check_class_hierarchy();
    }

    fn check_declaration(&mut self, declaration: &ast::Declaration) {
        use ast::Declaration::*;

        match declaration {
            DeclarationParseError => {
                panic!("should not see a parse error in first pass")
            },
            PackageDecl(_) => {
                self.check_package_location();
            },
            ImportDecl(names) => {
                let import_name = names.join(".");
                self.check_import_location(&import_name);
                self.check_import_duplication(import_name);
            },
            TypeDecl(tdecl, _tdef) => {
                self.saw_non_header_decl = true;
                self.check_duplicate_type_decl(*tdecl);
                // TODO: Check the validity of the defined type (references
                // defined types, no duplicate fields, etc)
                // TODO: If the type is a class, record information for checking
                // the class hierarchy and check method definition soundness
            },
            FunctionDecl(_func_decl) => {
                self.saw_non_header_decl = true;
                // TODO: Check arguments
                // TODO: Check function body
            },
            InstanceDecl(_inst_decl) => {
                self.saw_non_header_decl = true;
                // TODO: Check that it actually implements the class it says it does
            },
        }
    }

    fn check_package_location(&mut self) {
        if self.package_decl_set {
            self.add_error("duplicate package declaration");
        }
        if self.saw_first_decl {
            self.add_error("package declaration must be the first declaration in the file");
        }
        self.package_decl_set = true;
    }

    fn check_import_location(&mut self, import_name: &String) {
        if self.saw_non_header_decl {
            let err = format!(
                "import statement must be above other declarations: import {}",
                import_name);
            self.errors.push(err);
        }
    }

    fn check_import_duplication(&mut self, import_name: String) {
        if self.imports_used.contains(&import_name) {
            let err = format!("duplicate import of {}", import_name);
            self.errors.push(err);
        } else {
            self.imports_used.insert(import_name);
        }
    }

    fn check_duplicate_type_decl(&mut self, tdecl: ast::TypeRef) {
        let typ = self.syntax.get_type(tdecl);
        if let Some(name) = typ.declared_name() {
            if self.types_declared.contains(&name) {
                let err = format!("duplicate type declaration: {}", name);
                self.errors.push(err);
            } else {
                self.types_declared.insert(name);
            }
        } else {
            panic!("types in declarations should always have names");
        }
    }

    fn check_class_hierarchy(&mut self) {
        // TODO
    }

    fn add_error(&mut self, err: &str) {
        self.errors.push(err.to_owned());
    }
}
