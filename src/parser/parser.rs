use std::iter;

use super::ast::*;
use super::tokens::Token;
use super::location::Location;

pub fn parse(tokens: Vec<(Token, Location)>) -> Syntax {
    let mut result = Syntax::new();

    result
}

fn parse_statement<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> StatementRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let token = match cursor.peek() {
        None => {
            return syntax.add_statement(Statement::StatementParseError);
        },
        Some((t, _)) => t,
    };

    let result = match token {
        Token::KeywordReturn => {
            cursor.next();
            parse_return(cursor, syntax)
        },
        Token::KeywordLet => {
            cursor.next();
            // TODO: Let statement
            syntax.add_statement(Statement::StatementParseError)
        },
        Token::KeywordIf => {
            cursor.next();
            // TODO: If statement,
            syntax.add_statement(Statement::StatementParseError)
        },
        Token::KeywordWhile => {
            cursor.next();
            // TODO: While statement
            syntax.add_statement(Statement::StatementParseError)
        },
        Token::KeywordFor => {
            cursor.next();
            // TODO: For statement
            syntax.add_statement(Statement::StatementParseError)
        },
        Token::KeywordMatch => {
            cursor.next();
            // TODO: Match statement
            syntax.add_statement(Statement::StatementParseError)
        },
        Token::ValueName(name) => {
            // TODO: Parse assignment or expression
            syntax.add_statement(Statement::StatementParseError)
        },
        _ => {
            let expr = parse_expression(cursor, syntax);
            let stmt = Statement::ExprStmt(expr);
            syntax.add_statement(stmt)
        },
    };

    if require_next(Token::Newline, cursor) {
        result
    } else {
        syntax.add_statement(Statement::StatementParseError)
    }
}

fn parse_return<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> StatementRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let token = match cursor.peek() {
        None => {
            return syntax.add_statement(Statement::StatementParseError);
        },
        Some((t, _)) => t,
    };

    if *token == Token::Newline {
        // The caller eats this newline
        syntax.add_statement(Statement::Return)
    } else {
        let expr = parse_expression(cursor, syntax);
        syntax.add_statement(Statement::ReturnExpr(expr))
    }
}

// TODO: Handle the operators ^ & | ** << >>

// Operators at this level of precedence: ||
fn parse_expression<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> ExpressionRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let mut result = parse_precedence_1(cursor, syntax);
    while is_next(Token::DoubleOr, cursor) {
        cursor.next();
        let right = parse_precedence_1(cursor, syntax);
        let expr = Expression::BinaryOperator(BinaryOp::BoolOr, result, right);
        result = syntax.add_expression(expr);
    }
    result
}

// Operators at this level of precedence: &&
fn parse_precedence_1<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> ExpressionRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let mut result = parse_precedence_2(cursor, syntax);
    while is_next(Token::DoubleAnd, cursor) {
        cursor.next();
        let right = parse_precedence_2(cursor, syntax);
        let expr = Expression::BinaryOperator(BinaryOp::BoolAnd, result, right);
        result = syntax.add_expression(expr);
    }
    result
}

// Operators at this level of precedence: ==, !=
fn parse_precedence_2<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> ExpressionRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let mut result = parse_precedence_3(cursor, syntax);
    while let Some((token, _)) = cursor.peek() {
        let op = match *token {
            Token::DoubleEquals => BinaryOp::Equal,
            Token::NotEquals => BinaryOp::NotEqual,
            _ => {
                break;
            },
        };
        cursor.next();
        let right = parse_precedence_3(cursor, syntax);
        let expr = Expression::BinaryOperator(op, result, right);
        result = syntax.add_expression(expr);
    }
    result
}

// Operators at this level of precedence: <, <=, >, >=
fn parse_precedence_3<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> ExpressionRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let mut result = parse_precedence_4(cursor, syntax);
    while let Some((token, _)) = cursor.peek() {
        let op = match *token {
            Token::Less => BinaryOp::Less,
            Token::LessEquals => BinaryOp::LessEqual,
            Token::Greater => BinaryOp::Greater,
            Token::GreaterEquals => BinaryOp::GreaterEqual,
            _ => {
                break;
            },
        };
        cursor.next();
        let right = parse_precedence_4(cursor, syntax);
        let expr = Expression::BinaryOperator(op, result, right);
        result = syntax.add_expression(expr);
    }
    result
}

// Operators at this level of precedence: +, -
fn parse_precedence_4<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> ExpressionRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let mut result = parse_precedence_5(cursor, syntax);
    while let Some((token, _)) = cursor.peek() {
        let op = match *token {
            Token::Plus => BinaryOp::Plus,
            Token::Minus => BinaryOp::Minus,
            _ => {
                break;
            },
        };
        cursor.next();
        let right = parse_precedence_5(cursor, syntax);
        let expr = Expression::BinaryOperator(op, result, right);
        result = syntax.add_expression(expr);
    }
    result
}

// Operators at this level of precedence: *, /, %
fn parse_precedence_5<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> ExpressionRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let mut result = parse_unary(cursor, syntax);
    while let Some((token, _)) = cursor.peek() {
        let op = match *token {
            Token::Star => BinaryOp::Times,
            Token::Slash => BinaryOp::Divide,
            Token::Percent => BinaryOp::Mod,
            _ => {
                break;
            },
        };
        cursor.next();
        let right = parse_unary(cursor, syntax);
        let expr = Expression::BinaryOperator(op, result, right);
        result = syntax.add_expression(expr);
    }
    result
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

    fn assert_parses_expr(input: &str, expected: &str) {
        let mut s = Syntax::new();
        let eref = parse_expression(
            &mut tokenize(input).iter().peekable(),
            &mut s);
        let inspected = inspect(eref, &s).unwrap();
        assert_eq!(expected, inspected.as_str());
    }

    fn assert_parses_stmt(input: &str, expected: &str) {
        let mut s = Syntax::new();
        let sref = parse_statement(
            &mut tokenize(input).iter().peekable(),
            &mut s);
        let inspected = inspect(sref, &s).unwrap();
        assert_eq!(expected, inspected.as_str());
    }

    #[test]
    fn test_empty() {
        let result = parse(vec![]);
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
        assert_parses_stmt("return 123\n", "(return 123)");
        assert_parses_stmt("return 1 + 2\n", "(return (binary + 1 2))");
    }
}