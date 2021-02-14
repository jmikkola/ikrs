use super::super::tokenize::tokenize;
use super::*;

fn assert_parses_expr(input: &str, expected: &str) {
    let tokens = tokenize(input);
    let mut parser = Parser::new(&tokens);
    let eref = parser.parse_expression();
    let errors = parser.show_errors();
    let s = parser.syntax;
    let inspected = inspect(eref, &s).unwrap();
    assert_eq!(expected, inspected.as_str(), "{}", errors.join(", "));
}

fn assert_parses_stmt(input: &str, expected: &str) {
    let tokens = tokenize(input);
    let mut parser = Parser::new(&tokens);
    let sref = parser.parse_statement(None);
    let errors = parser.show_errors();
    let s = parser.syntax;
    let inspected = inspect(sref, &s).unwrap();
    assert_eq!(expected, inspected.as_str(), "{}", errors.join(", "));
}

#[test]
fn test_empty() {
    let result = parse(&vec![]);
    assert!(result.statements.is_empty());
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
    let stmt = "if 1:\n  return\nreturn    123\n";
    assert_parses_stmt(stmt, "(if 1 (do (return)))");
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
fn test_expression_statements() {
    assert_parses_stmt("x", "(expr x)");
    assert_parses_stmt("foo()", "(expr (call foo))");
}
