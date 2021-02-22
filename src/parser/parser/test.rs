use super::super::tokenize::tokenize;
use super::*;

fn assert_parses_type(input: &str, expected: &str) {
    assert_parses(input, expected, true, |parser| parser.parse_type());
}

fn assert_parses_expr(input: &str, expected: &str) {
    assert_parses(input, expected, true, |parser| parser.parse_expression());
}

fn assert_parses_pattern(input: &str, expected: &str) {
    assert_parses(input, expected, true, |parser| parser.parse_pattern());
}

fn assert_parses_stmt(input: &str, expected: &str) {
    let to_trim: &[_] = &[' ', '\n'];
    let trimmed = input.trim_start_matches(to_trim);
    assert_parses(trimmed, expected, true, |parser| parser.parse_statement(None));
}

fn assert_parses_decl(input: &str, expected: &str) {
    let to_trim: &[_] = &[' ', '\n'];
    let trimmed = input.trim_start_matches(to_trim);
    assert_parses(trimmed, expected, true, |parser| parser.parse_declaration(None));
}

fn assert_parses_file(input: &str, expected_decls: Vec<&str>) {
    let to_trim: &[_] = &[' ', '\n'];
    let trimmed = input.trim_start_matches(to_trim);

    let tokens = tokenize(input);
    let mut parser = Parser::new(&tokens.tokens);

    parser.parse_file();

    let is_done = parser.is_done();
    let s = &parser.syntax;
    let errors = &s.errors;

    let inspected: Vec<String> = s.declarations.iter()
        .map(|d| inspect(d, s).unwrap())
        .collect();

    assert_eq!(
        expected_decls, inspected,
        "input: {}, errors: {}", input, errors.join(", ")
    );
    assert_eq!("", errors.join(", "), "{}", input);
    assert_eq!(true, is_done, "parser left extra input");
}

fn assert_parses<F, I>(input: &str, expected: &str, require_done: bool, f: F)
where F: Fn(&mut Parser) -> I, I: Inspect {
    let tokens = tokenize(input);
    let mut parser = Parser::new(&tokens.tokens);
    let inspectable = f(&mut parser);
    let is_done = parser.is_done();
    let s = parser.syntax;
    let errors = &s.errors;
    let inspected = inspect(inspectable, &s).unwrap();
    assert_eq!(
        expected, inspected.as_str(),
        "input: {}, errors: {}", input, errors.join(", "),
    );
    assert_eq!("", errors.join(", "), "{}", input);
    if require_done {
        assert_eq!(true, is_done, "parser left extra input");
    }
}

#[test]
fn test_int() {
    assert_parses_expr("1234", "1234");
}

#[test]
fn test_variable() {
    assert_parses_expr("x", "x");
}

#[test]
fn test_structure_literal() {
    assert_parses_expr("None", "(make-struct None)");
    assert_parses_expr("None{}", "(make-struct None)");

    assert_parses_expr("Some{val: 123}", "(make-struct Some (val 123))");
    assert_parses_expr("Some{val: 123,}", "(make-struct Some (val 123))");
    assert_parses_expr("Some{\n\n  val: 123\n}", "(make-struct Some (val 123))");
}

#[test]
fn test_unary_bang() {
    assert_parses_expr("!1", "(unary ! 1)");
    assert_parses_expr(" ! 1 ", "(unary ! 1)");
}

#[test]
fn test_multiple_unary_operators() {
    let expected = "(unary ! (unary ! (unary ~ (unary - 3))))";
    assert_parses_expr("!!~-3", expected);
    assert_parses_expr(" ! ! ~ - 3 ", expected);
}

#[test]
fn test_parens() {
    assert_parses_expr("(99)", "(paren 99)");
}

#[test]
fn test_function_call_no_args() {
    assert_parses_expr("foo()", "(call foo)");
}

#[test]
fn test_function_call_one_arg() {
    assert_parses_expr("foo( 123 )", "(call foo 123)");
}

#[test]
fn test_function_call_trailing_comma() {
    assert_parses_expr("foo( 123, )", "(call foo 123)");
}

#[test]
fn test_function_call_many_args() {
    assert_parses_expr("foo( 1, 2, 3 )", "(call foo 1 2 3)");
}

#[test]
fn test_function_in_parens() {
    assert_parses_expr("(foo)()", "(call (paren foo))");
}

#[test]
fn test_offset_access() {
    assert_parses_expr("foo[123]", "(offset foo 123)");
}

#[test]
fn test_repeated_offset_access() {
    let expected = "(unary ~ (offset (offset foo 1) 2))";
    assert_parses_expr("~foo[1][2]", expected);
}

#[test]
fn test_field_access() {
    assert_parses_expr("foo.bar", "(access foo bar)");
}

#[test]
fn test_binary_ops() {
    let expected = "(binary - (binary - 10 3) 2)";
    assert_parses_expr("10 - 3 - 2", expected);
    assert_parses_expr("10-3-2", expected);
}

#[test]
fn test_star_operators() {
    assert_parses_expr("10 * 5", "(binary * 10 5)");
    assert_parses_expr("10 ** 5", "(binary ** 10 5)");
}

#[test]
fn test_mixing_binary_and_unary() {
    let expected = "(binary - 10 (unary - 5))";
    assert_parses_expr("10 - -5", expected);
    assert_parses_expr("10-- 5", expected);
    assert_parses_expr("10--5", expected);
}

#[test]
fn test_plain_return() {
    assert_parses_stmt("return\n", "(return)");
}

#[test]
fn test_return_with_expression() {
    assert_parses_stmt("return 123", "(return 123)");
    assert_parses_stmt("return 123\n", "(return 123)");
    assert_parses_stmt("return 1 + 2\n", "(return (binary + 1 2))");
}

#[test]
fn test_let_statement() {
    assert_parses_stmt("let a = 1 + 2\n", "(let a (binary + 1 2))");
}

#[test]
fn test_typed_let() {
    assert_parses_stmt("let a Int = 1", "(let a :: Int 1)");
}

#[test]
fn test_if_without_else() {
    let stmt = "if 1:\n  return\n";
    assert_parses_stmt(stmt, "(if 1 (do (return)))");
}

#[test]
fn test_if_with_two_statements() {
    let stmt = "if 1:\n  return\n  return    123\n";
    assert_parses_stmt(stmt, "(if 1 (do (return) (return 123)))");
}

#[test]
fn test_if_with_statement_after() {
    let input = "if 1:\n  return\nreturn    123\n";
    let expected = "(if 1 (do (return)))";
    assert_parses(input, expected, false, |parser| parser.parse_statement(None));
}

#[test]
fn test_if_else() {
    let stmt = "if 0 != 0:\n  return 1\nelse:\n  return 1\n  return 2\n";
    let expected = "(if (binary != 0 0) (do (return 1)) (do (return 1) (return 2)))";
    assert_parses_stmt(stmt, expected);
}

#[test]
fn test_nested_if_statements() {
    let stmt = "if 1:\n  if 2:\n    return 3\n  return 4";
    assert_parses_stmt(stmt, "(if 1 (do (if 2 (do (return 3))) (return 4)))");
}

#[test]
fn test_else_if() {
    let stmt = r#"
if x:
  return 1
else if y:
  return 2
"#;
    let expected = "(if x (do (return 1)) (if y (do (return 2))))";
    assert_parses_stmt(stmt, expected);
}

#[test]
fn test_else_if_else() {
    let stmt = r#"
if x:
  return 1
else if y:
  return 2
else:
  return 3
"#;
    let expected = "(if x (do (return 1)) (if y (do (return 2)) (do (return 3))))";
    assert_parses_stmt(stmt, expected);
}

#[test]
fn test_while() {
    let stmt = "while 1:\n  return 2";
    assert_parses_stmt(stmt, "(while 1 (do (return 2)))");
}

#[test]
fn test_for() {
    let stmt = "for a in x:\n  return a";
    assert_parses_stmt(stmt, "(for a in x (do (return a)))");
}

#[test]
fn test_assignment() {
    assert_parses_stmt("a = 5", "(assign a 5)");
}

#[test]
fn test_field_assignment() {
    assert_parses_stmt("foo.bar = 5", "(assign (access foo bar) 5)");
}

#[test]
fn test_offset_assignment() {
    assert_parses_stmt("arr[0] = 5", "(assign (offset arr 0) 5)");
}

#[test]
fn test_expression_statements() {
    assert_parses_stmt("x", "(expr x)");
    assert_parses_stmt("foo()", "(expr (call foo))");
}

#[test]
fn test_simple_patterns() {
    assert_parses_pattern("_", "wildcard");
    assert_parses_pattern("foo", "foo");
    assert_parses_pattern("123", "(lit 123)");
}

#[test]
fn test_named_pattern() {
    assert_parses_pattern("foo@1", "(@ foo (lit 1))");
    assert_parses_pattern("_@_", "(@ _ wildcard)");
}

#[test]
fn test_tuple_pattern() {
    assert_parses_pattern("()", "(tuple)");
    assert_parses_pattern("(1, 2, x)", "(tuple (lit 1) (lit 2) x)");
    // Allows a trailing comma
    assert_parses_pattern("(1, 2, x,)", "(tuple (lit 1) (lit 2) x)");
}

#[test]
fn test_struct_pattern() {
    assert_parses_pattern("None", "(match-struct None)");
    assert_parses_pattern("None()", "(match-struct None)");
    assert_parses_pattern("Some(_)", "(match-struct Some wildcard)");
    assert_parses_pattern("Some(_,)", "(match-struct Some wildcard)");
    assert_parses_pattern("Pair(1, 2)", "(match-struct Pair (lit 1) (lit 2))");
    assert_parses_pattern("Pair(1, 2,)", "(match-struct Pair (lit 1) (lit 2))");
}

#[test]
fn test_match() {
    let stmt = r#"
        match foo:
          None:
            x = 1
          Some(y):
            x = y
    "#;
    let expected = "(match foo (case (match-struct None) (do (assign x 1))) \
                    (case (match-struct Some y) (do (assign x y))))";
    assert_parses_stmt(stmt, expected);
}

#[test]
fn test_simple_type() {
    assert_parses_type("Int", "Int");
}

#[test]
fn test_type_with_generics() {
    assert_parses_type("Foo<>", "(generic Foo)");
    assert_parses_type("Foo<Bar,>", "(generic Foo Bar)");
    assert_parses_type("Foo<bar,>", "(generic Foo (tvar bar))");
    assert_parses_type("Result<Error, Option<Int>>", "(generic Result Error (generic Option Int))");
}

#[test]
fn test_void_type() {
    assert_parses_type("()", "void");
}

#[test]
fn test_type_variable() {
    assert_parses_type("a", "(tvar a)");
}

#[test]
fn test_function_type() {
    assert_parses_type("fn() ()", "(function () void)");
    assert_parses_type("fn(Int)", "(function (Int) void)");
    assert_parses_type("fn(Int) Bool", "(function (Int) Bool)");
    assert_parses_type("fn(A, B, C,) Bool", "(function (A B C) Bool)");
}

#[test]
fn test_function_type_with_constraints() {
    assert_parses_type(
        "fn(t) t where t: Sized",
        "(function ((tvar t)) (tvar t) where (((tvar t) Sized)))"
    );

    assert_parses_type(
        "fn(t) where t: Sized",
        "(function ((tvar t)) void where (((tvar t) Sized)))"
    );

    assert_parses_type(
        "fn(t) t where t: A, t: B",
        "(function ((tvar t)) (tvar t) where (((tvar t) A) ((tvar t) B)))"
    );
}

#[test]
fn test_package_decl() {
    assert_parses_decl("package main", "(package main)");
    assert_parses_decl("package main\n", "(package main)");
}

#[test]
fn test_import_decl() {
    assert_parses_decl("import foo", "(import foo)");
    assert_parses_decl("import foo.bar.baz", "(import foo bar baz)");
    assert_parses_decl("import foo.bar.baz\n", "(import foo bar baz)");
}

#[test]
fn test_function_decl() {
    let decl = r#"
fn main():
  println("Hello, world")
    "#;

    assert_parses_decl(decl, "(defn main () (do (expr (call println \"Hello, world\"))))");
}

#[test]
fn test_function_with_args() {
    let decl = r#"
fn add(a, b):
  return a + b
    "#;

    assert_parses_decl(decl, "(defn add (a b) (do (return (binary + a b))))");
}

#[test]
fn test_function_with_type() {
    let decl = r#"
fn add(a Int, b Int) Int:
  return a + b
    "#;

    assert_parses_decl(decl, "(defn add (a b) :: (function (Int Int) Int) (do (return (binary + a b))))");
}

#[test]
fn test_function_implicit_void_return() {
    let decl = r#"
fn save(a Int, b Int):
   foo(a, b)
    "#;

    assert_parses_decl(decl, "(defn save (a b) :: (function (Int Int) void) (do (expr (call foo a b))))");
}

#[test]
fn test_function_with_contraint() {
    let decl = r#"
fn save(a t) where t: Sized:
   foo(a)
    "#;

    let expected = "(defn save (a) \
                    :: (function ((tvar t)) void \
                    where (((tvar t) Sized))) \
                    (do (expr (call foo a))))";
    assert_parses_decl(decl, expected);
}

#[test]
fn test_struct_type_decl() {
    let decl = r#"
type Light struct:
  color Color
  value Int
    "#;

    let expected = "(type Light (struct (color Color) (value Int)))";
    assert_parses_decl(decl, expected);
}

#[test]
fn test_generic_struct_decl() {
    let decl = r#"
type Pair<t> struct:
  a t
  b t
    "#;

    let expected = "(type (generic Pair (tvar t)) (struct (a (tvar t)) (b (tvar t))))";
    assert_parses_decl(decl, expected);
}

#[test]
fn test_type_alias_decl() {
    let decl = "type Size Int";
    let expected = "(type Size alias Int)";
    assert_parses_decl(decl, expected);
}

#[test]
fn test_simple_enum() {
    let decl = r#"
type Color enum:
  Red
  Blue
  Green
"#;
    let expected = "(type Color (enum (Red) (Blue) (Green)))";
    assert_parses_decl(decl, expected);
}

#[test]
fn test_enum_with_fields() {
    let decl = r#"
type Lambdas enum:
  Variable:
    name String
  Abstraction:
    arg String
    body Lambdas
"#;

    let expected = "(type Lambdas (enum (Variable (name String)) \
                    (Abstraction (arg String) (body Lambdas))))";
    assert_parses_decl(decl, expected);
}

#[test]
fn test_class_definition() {
    let decl = r#"
type Addable class:
  fn add(Self, Self) Self
"#;

    let expected = "(type Addable (class (add :: (function (Self Self) Self))))";
    assert_parses_decl(decl, expected);
}

#[test]
fn test_class_extends() {
   let decl = r#"
type A class extends B, C:
  fn f(Self)
  fn g(Self, Int) String
"#;

    let expected = "(type A (class extends (B C) \
                    (f :: (function (Self) void)) \
                    (g :: (function (Self Int) String))))";
    assert_parses_decl(decl, expected);
}

#[test]
fn test_instance_declaration() {
    let decl = r#"
instance Show ID:
  fn show(self):
    return to_string(self)
"#;
    let expected = "(instance Show ID \
                    (defn show (self) (do (return (call to_string self)))))";
    assert_parses_decl(decl, expected);
}

#[test]
fn test_instance_with_constraints() {
    let decl = r#"
instance Show Pair<t> where t: Show:
  fn show(self):
    return show(self.left) + " " + show(self.right)
"#;
    let expected = "(instance Show \
                    (generic Pair (tvar t)) \
                    where (((tvar t) Show)) \
                    (defn show (self) (do (return (binary + (binary + \
                    (call show (access self left)) \" \") \
                    (call show (access self right)))))))";
    assert_parses_decl(decl, expected);
}

#[test]
fn test_class_definition_with_method_constraint() {
    let decl = r#"
type Addable class:
  fn add(Self, other) Self where other: ToInt
"#;

    let expected = "(type Addable (class (add :: (function (Self (tvar other)) Self \
                    where (((tvar other) ToInt))))))";
    assert_parses_decl(decl, expected);
}


#[test]
fn test_parses_file() {
    let file = r#"
package main

import fmt

type Coords struct:
  a Int
  b Int

fn foo(a):
  if 1:
    foo(a)

fn main():
  println("Hello, world")
"#;

    let expected = vec![
        "(package main)",
        "(import fmt)",
        "(type Coords (struct (a Int) (b Int)))",
        "(defn foo (a) (do (if 1 (do (expr (call foo a))))))",
        "(defn main () (do (expr (call println \"Hello, world\"))))",
    ];

    assert_parses_file(file, expected);
}

#[test]
fn test_parses_statement_after_enum() {
    let file = r#"
type E enum:
  V:
    x Int

fn main():
  return
"#;

    let expected = vec![
        "(type E (enum (V (x Int))))",
        "(defn main () (do (return)))",
    ];

    assert_parses_file(file, expected);
}
