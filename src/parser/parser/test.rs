use super::super::tokenize::tokenize;
use super::*;

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

fn assert_parses<F, I>(input: &str, expected: &str, require_done: bool, f: F)
where F: Fn(&mut Parser) -> I, I: Inspect {
    let tokens = tokenize(input);
    let mut parser = Parser::new(&tokens);
    let inspectable = f(&mut parser);
    let errors = parser.show_errors();
    let is_done = parser.is_done();
    let s = parser.syntax;
    let inspected = inspect(inspectable, &s).unwrap();
    assert_eq!(expected, inspected.as_str(), "{}", errors.join(", "));
    if require_done {
        assert_eq!(true, is_done, "parser left extra input");
    }
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
