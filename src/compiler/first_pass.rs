#![allow(clippy::comparison_chain)]
#![allow(clippy::match_like_matches_macro)]

use std::collections::HashMap;
use std::collections::HashSet;

use anyhow::{anyhow, Result, Error};

use crate::package::ParsedFile;
use crate::package::ParsedPackage;

use super::super::parser::ast;

pub fn check(package: &ParsedPackage) -> Result<()> {
    check_declared_names(package)?;

    for file in package.files.iter() {
        check_file(&file.syntax)?;
    }

    Ok(())
}

struct DeclaredNamesCheck {
    expected_name: String,
    allowed_to_skip_package_decl: bool,
    declared_names: HashMap<String, String>,
    imported_names: HashSet<String>,
    imported_names_by_file: HashSet<(String, String)>,
    imported_name_to_fqn_and_file: HashMap<String, (String, String)>,
    errors: Vec<Error>,
}

fn check_declared_names(package: &ParsedPackage) -> Result<()> {
    let mut check = DeclaredNamesCheck{
        expected_name: package.package_name.split('.').last().unwrap().into(),
        allowed_to_skip_package_decl: package.package_name == "main",
        declared_names: HashMap::new(),
        imported_names: HashSet::new(),
        imported_names_by_file: HashSet::new(),
        imported_name_to_fqn_and_file: HashMap::new(),
        errors: Vec::new(),
    };

    for file in package.files.iter() {
        check.check_package_decl(file);
        check.check_file_declared_names(file);
    }

    if check.errors.is_empty() {
        Ok(())
    } else {
        Err(check.errors.remove(0))
    }
}

impl DeclaredNamesCheck {
    fn check_file_declared_names(&mut self, file: &ParsedFile) {
        let syntax = &file.syntax;

        for declaration in syntax.declarations.iter() {
            use ast::Declaration::*;

            match declaration {
                PackageDecl(_) => {},
                ImportDecl(name_parts) => {
                    self.add_imported_name(file, name_parts);
                },
                TypeDecl(defined_type, _definition) => {
                    let t = syntax.get_type(*defined_type);

                    use ast::Type::*;

                    let declared_name = match t {
                        TypeName(name) => name.clone(),
                        Generic(name, _) => name.clone(),
                        _ => panic!("Compiler error: invalid type for LHS of type declaration {:?}", t),
                    };
                    self.add_declared_name(file, declared_name);
                },
                FunctionDecl(func_decl) => {
                    self.add_declared_name(file, func_decl.name.clone());
                },
                InstanceDecl(_inst_decl) => {
                    // TODO deal with duplicate instance declarations later
                },
                DeclarationParseError => {
                    panic!("Compiler error: Saw a DeclarationParseError");
                },
            }
        }
    }

    fn add_imported_name(&mut self, file: &ParsedFile, name_parts: &[String]) {
        let filename = file.filename();
        let imported_name = name_parts.last().unwrap().clone();

        self.imported_names.insert(imported_name.clone());

        // you can't import the same name twice in one file
        let key = (filename.clone(), imported_name.clone());
        if self.imported_names_by_file.contains(&key) {
            eprintln!("the name {} is imported multiple times in {}", imported_name, filename);
            self.errors.push(anyhow!("duplicate import of {}", imported_name));
            return;
        }
        self.imported_names_by_file.insert(key);

        // you can't import two different paths as the same name, even across different files in the
        // module.
        let fully_qualified_name: String = name_parts.to_vec().join(".");
        let existing: Option<(String, String)> = self.imported_name_to_fqn_and_file.get(&imported_name).cloned();
        if let Some((fqn, other_filename)) = existing {
            if fqn != fully_qualified_name {
                self.add_error(format!(
                    "{} already imported as {} in {}, cannot reimport {} in {}",
                    fqn, imported_name, other_filename, fully_qualified_name, filename,
                ));
                return;
            }
        }

        // you can't import a name that's the same as an existing declaration
        if let Some(other_file) = self.declared_names.get(&imported_name) {
            let error = format!(
                "{} declarted in {} has the same name as an import",
                imported_name, other_file,
            );
            self.add_error(error);
        }

        self.imported_name_to_fqn_and_file.insert(imported_name, (fully_qualified_name, filename));
    }

    fn add_declared_name(&mut self, file: &ParsedFile, name: String) {
        if let Some(other_file) = self.declared_names.get(&name) {
            eprintln!("duplicate declaration of {} in {} and {}", name, other_file, file.filename());
            self.errors.push(anyhow!("duplicate declaration of {}", name));
        } else if self.imported_names.contains(&name) {
            let error = format!("{} declarted in {} has the same name as an import", name, file.filename());
            self.add_error(error);
        } else {
            self.declared_names.insert(name, file.filename());
        }
    }

    fn add_error(&mut self, message: String) {
        eprintln!("{}", message);
        self.errors.push(anyhow!("{}", message));
    }

    // Check that the package name is the expected value (if it is provided)
    // or when not provided, make sure that is allowed.
    fn check_package_decl(&mut self, file: &ParsedFile) {
        let first_decl = if let Some(decl) = file.syntax.declarations.get(0) {
            decl
        } else {
            // ignore empty files
            return;
        };

        if let ast::Declaration::PackageDecl(name) = first_decl {
            if *name != self.expected_name {
                eprintln!(
                    "file {} should declare package {} but declares package {} instead",
                    file.syntax.filename, self.expected_name, name
                );
                self.errors.push(anyhow!("wrong package"));
            }
        } else if !self.allowed_to_skip_package_decl {
            eprintln!(
                "file {} should declare package {} but doesn't",
                file.syntax.filename, self.expected_name
            );
            self.errors.push(anyhow!("no package"));
        }
    }
}


// This performs several checks prior to type inference:
// - Declarations in a module declare unique names
// - Functions do not declare duplicate variables or arguments
// - Structure, class, and implementation declarations are well-formed
// - Multiple class definitions do not use overlapping method names
// - The class hierarchy is acyclic
// - The right number of generic args are passed to each type
// - And more
pub fn check_file(syntax: &ast::Syntax) -> Result<()> {
    let mut state = CheckState::new(syntax);
    state.check_syntax();
    state.print_result()
}

struct CheckState<'a> {
    syntax: &'a ast::Syntax,
    errors: Vec<String>,

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

        Err(anyhow!("{}", self.errors.first().unwrap()))
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
            if let TypeDecl(tdecl, _) = declaration {
                // as a side-effect, this writes the type decl to self.types_declared
                self.check_duplicate_type_decl(*tdecl);
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
        } else if self.saw_first_decl {
            self.add_error("package declaration must be the first declaration in the file");
        }

        self.package_decl_set = true;
    }

    fn check_import_location(&mut self, import_name: &str) {
        if self.saw_non_header_decl {
            let err = format!(
                "import statement must be above other declarations: import {}",
                import_name
            );
            self.errors.push(err);
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
                let class_name = self.syntax.get_type(defined).declared_name()
                    .expect("classes must have defined names");
                self.check_class_type(&class_name, class_type);
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
    fn check_class_type(&mut self, class_name: &str, class_type: &ast::ClassType) {
        // TODO: Record the class definition and its superclasses

        // Check for duplicated method names
        // O(n^2), but n should be small in sane code
        for (i, method1) in class_type.methods.iter().enumerate() {
            for (j, method2) in class_type.methods.iter().enumerate() {
                if i != j && method1.name == method2.name {
                    let err = format!("class {} has multiple definitions of method {}", class_name, method1.name);
                    self.errors.push(err);
                }
            }
        }

        // I don't _think_ it's necessary to ensure that the method names don't overlap with other
        // function names defined in the module, becuase the methods will be called with the
        // dot syntax (either value.method() or Type.method()), so it won't be ambiguous.

        for method in class_type.methods.iter() {
            self.check_class_method(class_name, method);
        }
    }

    fn check_class_method(&mut self, class_name: &str, method: &ast::ClassMethod) {
        let method_type = self.syntax.get_type(method.ftype);
        use ast::Type::*;

        match method_type {
            TypeParseError => {
                panic!("should not see a parse error in first pass");
            },
            FnType(_) => {
                // Self is a special type defined in class definitions that refers to the
                // implementing type
                let mut type_context = vec!["Self".to_string()];
                self.ensure_type_is_defined_with_context(method.ftype, &mut type_context);
                // TODO: Ensure no constraints are applied to the Self type
            },
            _ => {
                panic!("invalid type stored for a class method on class {}: {:?}", class_name, method_type);
            },
        }
    }

    fn ensure_type_is_defined(&mut self, tref: ast::TypeRef) {
        let mut type_context = Vec::new();
        self.ensure_type_is_defined_with_context(tref, &mut type_context);
    }

    fn ensure_type_is_defined_with_context(&mut self, tref: ast::TypeRef, type_context: &mut Vec<String>) {
        use ast::Type::*;

        let typ = self.syntax.get_type(tref);

        match typ {
            TypeParseError => {
                panic!("should not see a parse error in first pass")
            }
            Void => {}
            TypeName(name) => {
                if !self.is_type_defined(name, type_context) {
                    let err = format!("undefined type: {}", name);
                    self.errors.push(err);
                }
            }
            TypeVar(_var) => {
                // TODO: Ensure type vars are in scope
            }
            Generic(name, trefs) => {
                if !self.is_type_defined(name, type_context) {
                    let err = format!("undefined type: {}", name);
                    self.errors.push(err);
                }
                for tref in trefs.iter() {
                    self.ensure_type_is_defined_with_context(*tref, type_context);
                }
            }
            FnType(func_type) => {
                for tref in func_type.arg_types.iter() {
                    self.ensure_type_is_defined_with_context(*tref, type_context);
                }
                self.ensure_type_is_defined_with_context(func_type.ret_type, type_context);
                // TODO: Ensure func_type.constraints are defined classes
            }
        }
    }

    fn check_class_hierarchy(&mut self) {
        // TODO
    }

    fn is_type_defined(&self, typ: &str, type_context: &[String]) -> bool {
        self.is_type_builtin(typ) || self.is_type_defined_in_module(typ) || type_context.contains(&typ.into())
    }

    fn is_type_defined_in_module(&self, typ: &str) -> bool {
        self.types_declared.contains(typ)
    }

    fn is_type_builtin(&self, typ: &str) -> bool {
        match typ {
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
    use crate::parser::test_helper::tokenize_and_parse_with_name;

    fn parse_nth_file(n: usize,  file: &str) -> ast::Syntax {
        let name = format!("file_{}.ikko", n);
        tokenize_and_parse_with_name(file, &name).expect("cannot parse example in test")
    }

    fn get_error_from_check(package_name: &str, files: &[&str]) -> Option<String> {
        let syntaxes = files.iter().enumerate().map(|(idx, file)| parse_nth_file(idx, *file));
        let parsed_files = syntaxes.map(|syntax| ParsedFile{syntax}).collect();
        let package = ParsedPackage{
            package_name: package_name.into(),
            files: parsed_files,
        };
        match check(&package) {
            Ok(_) => None,
            Err(err) => Some(format!("{}", err)),
        }
    }

    fn error_matches(error: &str, pattern: &str) -> bool {
        let re = Regex::new(pattern).unwrap();
        re.is_match(error)
    }

    fn expect_package_ok(package_name: &str, files: &[&str]) {
        let error = get_error_from_check(package_name, files);
        assert!(error.is_none(), "{:?}", error);
    }

    fn expect_package_has_error(package_name: &str, files: &[&str], error_pattern: &str) {
        let error = get_error_from_check(package_name, files);
        match error {
            None => panic!("expected an error, got none"),
            Some(message) => assert!(
                error_matches(&message, error_pattern),
                "expected error {:?} to match the pattern {}",
                message, error_pattern,
            ),
        }
    }

    #[test]
    fn test_valid_package() {
        let file = r#"
package foo

import a.b
"#;
        expect_package_ok("foo", &[file]);
    }

    #[test]
    fn test_duplicate_package() {
        let file = r#"
package foo
package foo
"#;
        expect_package_has_error("foo", &[file], r"duplicate package declaration");
    }

    #[test]
    fn test_package_after_other_statements() {
        let file = r#"
import a
package foo
"#;
        expect_package_has_error(
            "main",
            &[file],
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
        expect_package_ok("main", &[file]);
    }

    #[test]
    fn test_duplicate_imports() {
        let file = r#"
import a.b.c
import a.b.c
"#;
        expect_package_has_error("main", &[file], r"duplicate import");
    }

    #[test]
    fn test_two_imports_using_the_same_name() {
        let file = r#"
import a.b.c
import x.y.c
"#;
        expect_package_has_error("main", &[file], r"duplicate import");
    }

    #[test]
    fn test_two_files_with_same_import() {
        let file1 = "import a.b.c";
        let file2 = "import a.b.c";
        expect_package_ok("main", &[file1, file2]);
    }

    #[test]
    fn test_two_files_with_conflicting_import() {
        let file1 = "import a.b.c";
        let file2 = "import x.y.c";
        expect_package_has_error("main", &[file1, file2], r"already imported");
    }

    #[test]
    fn test_two_files_with_conflict_between_import_and_declaration() {
        let file1 = "import x.foo";
        let file2 = r#"
fn foo():
  return
"#;
        // same result regardless of the order of the files
        expect_package_has_error("main", &[file1, file2], "same name as an import");
        expect_package_has_error("main", &[file2, file1], "same name as an import");
    }

    #[test]
    fn test_import_after_other_declarations() {
        let file = r#"
import a
fn foo():
  return
import b
"#;
        expect_package_has_error(
            "main",
            &[file],
            r"import statement must be above other declarations: import b",
        );
    }

    #[test]
    fn test_valid_type_declaration() {
        let file = r#"
type Length Int
type Name String
"#;
        expect_package_ok("main", &[file]);
    }

    #[test]
    fn test_duplicate_type_declaration() {
        let file = r#"
type Length Int
type Length Int
"#;
        expect_package_has_error("main", &[file], r"duplicate declaration of Length");
    }

    #[test]
    fn test_valid_reference_type_defined_in_module() {
        let file = r#"
type B A
type A Int
"#;
        expect_package_ok("main", &[file]);
    }

    #[test]
    fn test_alias_to_undefined_type() {
        let file = r#"
type A Int
type B C
"#;
        expect_package_has_error("main", &[file], r"undefined type: C");
    }

    #[test]
    fn test_valid_struct_definition() {
        let file = r#"
type B<t> struct:
  t t
  a A
type A Int
"#;
        expect_package_ok("main", &[file]);
    }

    #[test]
    fn test_valid_self_referential_struct() {
        let file = r#"
type B struct:
  value Int
  next B
"#;
        expect_package_ok("main", &[file]);
    }

    #[test]
    fn test_struct_with_undefined_type() {
        let file = r#"
type A struct:
  foo C
"#;
        expect_package_has_error("main", &[file], r"undefined type: C");
    }

    #[test]
    fn test_struct_involving_undefined_type() {
        let file = r#"
type A struct:
  foo fn(C) Int
"#;
        expect_package_has_error("main", &[file], r"undefined type: C");
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
        expect_package_has_error("main", &[file], r"undefined type: C");
    }

    #[test]
    fn test_cannot_redefined_builtin_type() {
        let file = r#"
type String Int
"#;
        expect_package_has_error("main", &[file], r"cannot redefine builtin type String");
    }

    #[test]
    fn test_class_with_multiple_methods() {
        let file = r#"
type Shape class:
  fn area(Self) Int

  fn color(Self) String
"#;
        expect_package_ok("main", &[file]);
    }

    #[test]
    fn test_class_with_duplicate_method_names() {
        let file = r#"
type Shape class:
  fn area(Self) Int
  fn color(Self) String
  fn area(Self) Float
"#;
        expect_package_has_error(
            "main", &[file],
            r"class Shape has multiple definitions of method area"
        );
    }

    #[test]
    fn test_class_referencing_undefined_type() {
        let file = r#"
type Shape class:
  fn color(Self) Color
"#;
        expect_package_has_error("main", &[file], r"undefined type: Color");
    }

    #[test]
    fn test_class_referencing_type_defined_later() {
        let file = r#"
type Shape class:
  fn color(Self) Color

type Color enum:
  Red
  Blue
  Green
"#;
        expect_package_ok("main", &[file]);
    }

    // TODO
    //     #[test]
    //     fn test_recursive_alias() {
    //         let file = r#"
    // type A A
    // "#;
    //         expect_package_has_error("main", &[file], r"self-referential type alias");
    //     }
}
