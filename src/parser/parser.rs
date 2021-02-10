use std::iter;

use super::ast::*;
use super::tokens::Token;
use super::location::Location;

pub fn parse(tokens: Vec<(Token, Location)>) -> Syntax {
    let mut result = Syntax::new();

    result
}

fn parse_expression<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> ExpressionRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    parse_unary(cursor, syntax)
}

// Unary operators and the value they apply to
fn parse_unary<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> ExpressionRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let token = match cursor.peek() {
        None => {
            return syntax.add_expression(Expression::ExpressionParseError)
        },
        Some((t, _)) => t,
    };

    match token {
        Token::Bang => {
            cursor.next();
            let inner = parse_unary(cursor, syntax);
            let op = UnaryOp::BoolNot;
            let expr = Expression::UnaryOperator(op, inner);
            syntax.add_expression(expr)
        },
        Token::Tilda => {
            cursor.next();
            let inner = parse_unary(cursor, syntax);
            let op = UnaryOp::BitInvert;
            let expr = Expression::UnaryOperator(op, inner);
            syntax.add_expression(expr)
        },
        Token::Minus => {
            cursor.next();
            let inner = parse_unary(cursor, syntax);
            let op = UnaryOp::Negate;
            let expr = Expression::UnaryOperator(op, inner);
            syntax.add_expression(expr)
        },
        _ => {
            parse_term(cursor, syntax)
        },
    }
}

// Single values and field access, array access, and function calls
fn parse_term<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> ExpressionRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let mut value = parse_single_value(cursor, syntax);

    // This loop handles zero or more field access, offet access, and/or
    // function arguments after an expression.
    // E.g. if the expression is `x.foo[1].bar()`, then `x` is the current value
    // and each time though the loop picks up another one of `.foo`, `[1]`,
    // `.bar`, and `()`.
    loop {
        value = match cursor.peek() {
            Some((Token::LParen, _)) => {
                cursor.next();
                let mut args = vec![];

                // Read args passed to function, allowing a trailing comma
                while !is_next(Token::RParen, cursor) {
                    args.push(parse_expression(cursor, syntax));
                    if is_next(Token::Comma, cursor) {
                        cursor.next();
                    } else {
                        break;
                    }
                }

                if require_next(Token::RParen, cursor) {
                    syntax.add_expression(Expression::FunctionCall(value, args))
                } else {
                    syntax.add_expression(Expression::ExpressionParseError)
                }
            },
            Some((Token::LBracket, _)) => {
                cursor.next();
                let offset = parse_expression(cursor, syntax);

                if require_next(Token::RBracket, cursor) {
                    syntax.add_expression(Expression::OffsetAccess(value, offset))
                } else {
                    syntax.add_expression(Expression::ExpressionParseError)
                }
            },
            Some((Token::Dot, _)) => {
                cursor.next();
                // expecting a field name
                match cursor.next() {
                    Some((Token::ValueName(s), _)) => {
                        syntax.add_expression(Expression::FieldAccess(value, s.clone()))
                    },
                    _ => {
                        syntax.add_expression(Expression::ExpressionParseError)
                    }
                }
            },
            _ => {
                return value
            },
        }
    }
}

// Literals, variables, and parentheticals
fn parse_single_value<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> ExpressionRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let token = match cursor.next() {
        None => {
            return syntax.add_expression(Expression::ExpressionParseError)
        },
        Some((t, _)) => t,
    };

    match token {
        Token::LParen => {
            let inner = parse_expression(cursor, syntax);
            if require_next(Token::RParen, cursor) {
                syntax.add_expression(Expression::Paren(inner))
            } else {
                syntax.add_expression(Expression::ExpressionParseError)
            }
        },
        Token::IntLiteral(i) => {
            let expr = Expression::Literal(Literal::Integer(*i));
            syntax.add_expression(expr)
        },
        Token::FloatLiteral(f) => {
            let expr = Expression::Literal(Literal::Float(*f));
            syntax.add_expression(expr)
        },
        Token::StringLiteral(s) => {
            let expr = Expression::Literal(Literal::String(s.clone()));
            syntax.add_expression(expr)
        },
        Token::ValueName(s) => {
            let expr = Expression::Variable(s.clone());
            syntax.add_expression(expr)
        },
        _ => syntax.add_expression(Expression::ExpressionParseError),
    }
}

fn require_next<'a, I>(expected: Token, cursor: &mut iter::Peekable<I>) -> bool
where I: iter::Iterator<Item=&'a (Token, Location)> {
    match cursor.next() {
        Some((token, _)) => *token == expected,
        None => false,
    }
}

fn is_next<'a, I>(expected: Token, cursor: &mut iter::Peekable<I>) -> bool
where I: iter::Iterator<Item=&'a (Token, Location)> {
    match cursor.peek() {
        Some((token, _)) => *token == expected,
        None => false,
    }
}


#[cfg(test)]
mod test {
    use super::super::tokenize::tokenize;
    use super::*;

    fn get_expression(input: &str) -> (Syntax, ExpressionRef) {
        let tokens = tokenize(input);

        let mut result = Syntax::new();
        let mut cursor = tokens.iter().peekable();
        let expr_ref = parse_expression(&mut cursor, &mut result);

        // assert that the iterator is done
        assert_eq!(None, cursor.peek());

        (result, expr_ref)
    }

    fn assert_parses_expr(syntax: &Syntax, eref: ExpressionRef, text: &str) {
        let (result_s, result_ref) = get_expression(text);
        let message = format!("expected: {:?}, actual: {:?}", syntax, result_s);
        assert!(syntax.expression_equals(eref, &result_s, result_ref), message);
    }

    fn l_int(i: i64) -> Expression {
        Expression::Literal(Literal::Integer(i))
    }

    fn var(name: &str) -> Expression {
        Expression::Variable(name.to_string())
    }

    #[test]
    fn test_empty() {
        let result = parse(vec![]);
        assert!(result.statements.is_empty());
    }

    #[test]
    fn test_int() {
        let mut s = Syntax::new();
        let eref = s.add_expression(l_int(1234));
        assert_parses_expr(&s, eref, "1234");
    }

    #[test]
    fn test_variable() {
        let mut s = Syntax::new();
        let eref = s.add_expression(var("x"));
        assert_parses_expr(&s, eref, "x");
    }

    #[test]
    fn test_unary_bang() {
        let mut s = Syntax::new();
        let inner = s.add_expression(l_int(1));
        let eref = s.add_expression(Expression::UnaryOperator(UnaryOp::BoolNot, inner));
        assert_parses_expr(&s, eref, "!1");
        assert_parses_expr(&s, eref, "  !  1  ");
    }

    #[test]
    fn test_multiple_unary_operators() {
        let mut s = Syntax::new();
        let mut eref = s.add_expression(l_int(3));
        use UnaryOp::*;
        for op in vec![Negate, BitInvert, BoolNot, BoolNot] {
            let expr = Expression::UnaryOperator(op, eref);
            eref = s.add_expression(expr);
        }
        assert_parses_expr(&s, eref, "!!~-3");
        assert_parses_expr(&s, eref, " ! ! ~ - 3 ");
    }

    #[test]
    fn test_parens() {
        let mut s = Syntax::new();
        let inner = s.add_expression(l_int(99));
        let eref = s.add_expression(Expression::Paren(inner));
        assert_parses_expr(&s, eref, "(99)");
    }

    #[test]
    fn test_function_call_no_args() {
        let mut s = Syntax::new();
        let foo = s.add_expression(var("foo"));
        let eref = s.add_expression(Expression::FunctionCall(foo, vec![]));
        assert_parses_expr(&s, eref, "foo()");
    }

    #[test]
    fn test_function_call_one_arg() {
        let mut s = Syntax::new();
        let foo = s.add_expression(var("foo"));
        let arg = s.add_expression(l_int(123));
        let eref = s.add_expression(Expression::FunctionCall(foo, vec![arg]));
        assert_parses_expr(&s, eref, "foo( 123 )");
    }

    #[test]
    fn test_function_call_trailing_comma() {
        let mut s = Syntax::new();
        let foo = s.add_expression(var("foo"));
        let arg = s.add_expression(l_int(123));
        let eref = s.add_expression(Expression::FunctionCall(foo, vec![arg]));
        assert_parses_expr(&s, eref, "foo( 123, )");
    }

    #[test]
    fn test_function_call_many_args() {
        let mut s = Syntax::new();
        let foo = s.add_expression(var("foo"));
        let args = vec![
            s.add_expression(l_int(1)),
            s.add_expression(l_int(2)),
            s.add_expression(l_int(3)),
        ];
        let eref = s.add_expression(Expression::FunctionCall(foo, args));
        assert_parses_expr(&s, eref, "foo( 1, 2, 3 )");
    }

    #[test]
    fn test_function_in_parens() {
        let mut s = Syntax::new();
        let mut foo = s.add_expression(var("foo"));
        foo = s.add_expression(Expression::Paren(foo));
        let eref = s.add_expression(Expression::FunctionCall(foo, vec![]));
        assert_parses_expr(&s, eref, "(foo)()");
    }

    #[test]
    fn test_offset_access() {
        let mut s = Syntax::new();
        let foo = s.add_expression(var("foo"));
        let index = s.add_expression(l_int(123));
        let eref = s.add_expression(Expression::OffsetAccess(foo, index));
        assert_parses_expr(&s, eref, "foo[123]");
    }

    #[test]
    fn test_repeated_offset_access() {
        let mut s = Syntax::new();
        let mut eref = s.add_expression(var("foo"));
        let index1 = s.add_expression(l_int(1));
        eref = s.add_expression(Expression::OffsetAccess(eref, index1));
        let index2 = s.add_expression(l_int(2));
        eref = s.add_expression(Expression::OffsetAccess(eref, index2));
        eref = s.add_expression(Expression::UnaryOperator(UnaryOp::BitInvert, eref));
        assert_parses_expr(&s, eref, "~foo[1][2]");
    }

    #[test]
    fn test_field_access() {
        let mut s = Syntax::new();
        let foo = s.add_expression(var("foo"));
        let eref = s.add_expression(Expression::FieldAccess(foo, "bar".to_string()));
        assert_parses_expr(&s, eref, "foo.bar");
    }
}
