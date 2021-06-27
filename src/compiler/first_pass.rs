use std::collections::HashSet;

use anyhow::{anyhow, Result};

use super::super::parser::ast;

// This performs several checks prior to type inference:
// - Declarations in a module declare unique names
// - Functions do not declare duplicate variables or arguments
// - Structure, class, and implementation declarations are well-formed
// - Multiple class definitions do not use overlapping method names
// - The class hierarchy is acyclic
// - The right number of generic args are passed to each type
// - And more
pub fn check(syntax: &ast::Syntax) -> Result<()> {
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
        CheckState {
            syntax,
            errors: Vec::new(),

            // binding_names: HashSet::new(),
            imports_used: HashSet::new(),
            types_declared: HashSet::new(),

            package_decl_set: false,
            saw_first_decl: false,
            saw_non_header_decl: false,
        }
    }

    fn print_result(&self) -> Result<()> {
        if self.errors.is_empty() {
            return Ok(());
        }

        for error in self.errors.iter() {
            println!("{}", error);
        }

        Err(anyhow!("checks failed"))
    }

    fn check_syntax(&mut self) {
        self.gather_defined_types();
        self.check_declarations();
        self.check_class_hierarchy();
    }

    // Gather the names of defined types before checking declarations so we can
    // tell if they refer to any types that don't actually exist.
    fn gather_defined_types(&mut self) {
        use ast::Declaration::*;

        for declaration in self.syntax.declarations.iter() {
            match declaration {
                TypeDecl(tdecl, _) => {
                    // as a side-effect, this writes the type decl to
                    // self.types_declared
                    self.check_duplicate_type_decl(*tdecl);
                }
                _ => {}
            }
        }
    }

    fn check_declarations(&mut self) {
        for declaration in self.syntax.declarations.iter() {
            self.check_declaration(declaration);
            self.saw_first_decl = true;
        }
    }

    fn check_declaration(&mut self, declaration: &ast::Declaration) {
        use ast::Declaration::*;

        match declaration {
            DeclarationParseError => {
                panic!("should not see a parse error in first pass")
            }
            PackageDecl(_) => {
                self.check_package_location();
            }
            ImportDecl(names) => {
                let import_name = names.join(".");
                self.check_import_location(&import_name);
                self.check_import_duplication(import_name);
            }
            TypeDecl(tdecl, tdef) => {
                self.saw_non_header_decl = true;
                self.check_defined_type(*tdecl, &*tdef);
            }
            FunctionDecl(_func_decl) => {
                self.saw_non_header_decl = true;
                // TODO: Check arguments/constraints
                // TODO: Check function body
            }
            InstanceDecl(_inst_decl) => {
                self.saw_non_header_decl = true;
                // TODO: Check that it actually implements the class it says it does
            }
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
                import_name
            );
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
            if self.is_type_builtin(&name) {
                let err = format!("cannot redefine builtin type {}", name);
                self.errors.push(err);
            } else if self.types_declared.contains(&name) {
                let err = format!("duplicate type declaration: {}", name);
                self.errors.push(err);
            } else {
                self.types_declared.insert(name);
            }
        } else {
            panic!("types in declarations should always have names");
        }
    }

    fn check_defined_type(&mut self, defined: ast::TypeRef, tdef: &ast::TypeDefinition) {
        use ast::TypeDefinition::*;

        match tdef {
            Alias(tref) => {
                self.check_alias_type(defined, *tref);
            }
            Structure(struct_type) => {
                self.check_struct_type(struct_type);
            }
            Enum(enum_type) => {
                self.check_enum_type(enum_type);
            }
            Class(class_type) => {
                self.check_class_type(class_type);
            }
        }
    }

    fn check_alias_type(&mut self, defined: ast::TypeRef, tref: ast::TypeRef) {
        self.ensure_type_is_defined(tref);
        // Check if this is a self-referential alias
        // (e.g. `type A A`, or `type A Option<A>`)
        // TODO: This should really also disallow alias cycles
        // (e.g. `type A B; type B A`)

        let _name = self
            .syntax
            .get_type(defined)
            .declared_name()
            .expect("should have already checked this in check_duplicate_type_decl");
    }

    fn check_struct_type(&mut self, struct_type: &ast::StructType) {
        let mut fields = HashSet::new();
        for (name, tref) in struct_type.fields.iter() {
            if !fields.insert(name.clone()) {
                let err = format!("duplicate struct field: {}", name);
                self.errors.push(err);
            }
            self.ensure_type_is_defined(*tref);
        }
    }

    // Check that:
    // * variant names are not duplicated
    // * fields within a variant are not duplicated
    // * types in a field are defined
    // * (TODO) variant names do not overlap with defined types
    fn check_enum_type(&mut self, enum_type: &ast::EnumType) {
        let mut variants = HashSet::new();
        for variant in enum_type.variants.iter() {
            if !variants.insert(variant.name.clone()) {
                let err = format!("duplicate enum variant: {}", variant.name);
                self.errors.push(err);
            }

            let mut fields = HashSet::new();
            for (name, tref) in variant.content.fields.iter() {
                if !fields.insert(name.clone()) {
                    let err = format!("duplicate enum field: {} in variant {}", name, variant.name);
                    self.errors.push(err);
                }
                self.ensure_type_is_defined(*tref);
            }
        }

        // TODO: Ensure variant names are not defined types
    }

    // TODO: If the type is a class, record information for checking
    // the class hierarchy and check method definition soundness
    fn check_class_type(&mut self, _class_type: &ast::ClassType) {
        // TODO: Record the class definition and its superclasses
        // TODO: Ensure method names are not duplicated
        // TODO: Ensure method names do not overlap with other defined function names
        // TODO: Check argument types/constraints on the methods
    }

    fn ensure_type_is_defined(&mut self, tref: ast::TypeRef) {
        use ast::Type::*;

        let typ = self.syntax.get_type(tref);

        match typ {
            TypeParseError => {
                panic!("should not see a parse error in first pass")
            }
            Void => {}
            TypeName(name) => {
                if !self.is_type_defined(name) {
                    let err = format!("undefined type: {}", name);
                    self.errors.push(err);
                }
            }
            TypeVar(_var) => {
                // TODO: Ensure type vars are in scope
            }
            Generic(name, trefs) => {
                if !self.is_type_defined(name) {
                    let err = format!("undefined type: {}", name);
                    self.errors.push(err);
                }
                for tref in trefs.iter() {
                    self.ensure_type_is_defined(*tref);
                }
            }
            FnType(func_type) => {
                for tref in func_type.arg_types.iter() {
                    self.ensure_type_is_defined(*tref);
                }
                self.ensure_type_is_defined(func_type.ret_type);
                // TODO: Ensure func_type.constraints are defined classes
            }
        }
    }

    fn check_class_hierarchy(&mut self) {
        // TODO
    }

    fn is_type_defined(&self, typ: &String) -> bool {
        self.is_type_builtin(typ) || self.is_type_defined_in_module(typ)
    }

    fn is_type_defined_in_module(&self, typ: &String) -> bool {
        self.types_declared.contains(typ)
    }

    fn is_type_builtin(&self, typ: &String) -> bool {
        match typ.as_str() {
            "Bool" => true,
            "Char" => true,
            "Float" => true,
            "Int" => true,
            "Option" => true,
            "String" => true,
            _ => false,
        }
    }

    fn add_error(&mut self, err: &str) {
        self.errors.push(err.to_owned());
    }
}

#[cfg(test)]
mod test {
    use regex::Regex;

    use super::*;
    use crate::parser::test_helper::tokenize_and_parse;

    fn must_parse(file: &str) -> ast::Syntax {
        tokenize_and_parse(file).expect("cannot parse example in test")
    }

    fn get_errors<'a>(file: &str) -> Vec<String> {
        let syntax = must_parse(file);
        let mut state = CheckState::new(&syntax);
        state.check_syntax();
        state.errors
    }

    fn errors_match(errors: &Vec<String>, pattern: &str) -> bool {
        let re = Regex::new(pattern).unwrap();
        for err in errors.iter() {
            if re.is_match(err) {
                return true;
            }
        }
        false
    }

    fn expect_ok(file: &str) {
        assert!(get_errors(file).is_empty());
    }

    fn expect_has_error(file: &str, pattern: &str) {
        let errors = get_errors(file);
        assert!(
            errors_match(&errors, pattern),
            "expected errors {:?} to match pattern {}",
            errors,
            pattern
        );
    }

    #[test]
    fn test_valid_package() {
        let file = r#"
package foo

import a.b
"#;
        expect_ok(file);
    }

    #[test]
    fn test_duplicate_package() {
        let file = r#"
package foo
package foo
"#;
        expect_has_error(file, r"duplicate package declaration");
    }

    #[test]
    fn test_package_after_other_statements() {
        let file = r#"
import a
package foo
"#;
        expect_has_error(
            file,
            r"package declaration must be the first declaration in the file",
        );
    }

    #[test]
    fn test_valid_imports() {
        let file = r#"
import a
import a.b
import b.c
import bc
"#;
        expect_ok(file);
    }

    #[test]
    fn test_duplicate_imports() {
        let file = r#"
import a.b.c
import a.b.c
"#;
        expect_has_error(file, r"duplicate import");
    }

    #[test]
    fn test_import_after_other_declarations() {
        let file = r#"
import a
fn foo():
  return
import b
"#;
        expect_has_error(
            file,
            r"import statement must be above other declarations: import b",
        );
    }

    #[test]
    fn test_valid_type_declaration() {
        let file = r#"
type Length Int
type Name String
"#;
        expect_ok(file);
    }

    #[test]
    fn test_duplicate_type_declaration() {
        let file = r#"
type Length Int
type Length Int
"#;
        expect_has_error(file, r"duplicate type declaration: Length");
    }

    #[test]
    fn test_valid_reference_type_defined_in_module() {
        let file = r#"
type B A
type A Int
"#;
        expect_ok(file);
    }

    #[test]
    fn test_alias_to_undefined_type() {
        let file = r#"
type A Int
type B C
"#;
        expect_has_error(file, r"undefined type: C");
    }

    #[test]
    fn test_valid_struct_definition() {
        let file = r#"
type B<t> struct:
  t t
  a A
type A Int
"#;
        expect_ok(file);
    }

    #[test]
    fn test_valid_self_referential_struct() {
        let file = r#"
type B struct:
  value Int
  next B
"#;
        expect_ok(file);
    }

    #[test]
    fn test_struct_with_undefined_type() {
        let file = r#"
type A struct:
  foo C
"#;
        expect_has_error(file, r"undefined type: C");
    }

    #[test]
    fn test_struct_involving_undefined_type() {
        let file = r#"
type A struct:
  foo fn(C) Int
"#;
        expect_has_error(file, r"undefined type: C");
    }

    #[test]
    fn test_enum_referencing_undefined_type() {
        let file = r#"
type A enum:
  Left:
    x Int
  Right:
    y C
"#;
        expect_has_error(file, r"undefined type: C");
    }

    #[test]
    fn test_cannot_redefined_builtin_type() {
        let file = r#"
type String Int
"#;
        expect_has_error(file, r"cannot redefine builtin type String");
    }

    // TODO
    //     #[test]
    //     fn test_recursive_alias() {
    //         let file = r#"
    // type A A
    // "#;
    //         expect_has_error(file, r"self-referential type alias");
    //     }
}
