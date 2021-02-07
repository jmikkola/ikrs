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

// TODO: Add a layer above parse_unary for handling function calls, array access, and field access
// e.g. foo[i](x)[j](y).c, or (bar).baz
// This might involve splitting parse_unary so that e.g. !foo(x) parses as (!
// (foo x)) rather than ((! foo) x).
// - Bottom: paren, literal, name
// - Next level: array access, field access, function call
// - Top level: unary operators

fn parse_unary<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> ExpressionRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    match cursor.next() {
        None => syntax.add_expression(Expression::ExpressionParseError),
        Some((token, _)) => match token {
            Token::LParen => {
                let inner = parse_expression(cursor, syntax);
                match cursor.next() {
                    Some((Token::RParen, _)) => {
                        syntax.add_expression(Expression::Paren(inner))
                    },
                    _ => {
                        syntax.add_expression(Expression::ExpressionParseError)
                    },
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
            Token::Bang => {
                let inner = parse_unary(cursor, syntax);
                let op = UnaryOp::BoolNot;
                let expr = Expression::UnaryOperator(op, inner);
                syntax.add_expression(expr)
            },
            Token::Tilda => {
                let inner = parse_unary(cursor, syntax);
                let op = UnaryOp::BitInvert;
                let expr = Expression::UnaryOperator(op, inner);
                syntax.add_expression(expr)
            },
            Token::Minus => {
                let inner = parse_unary(cursor, syntax);
                let op = UnaryOp::Negate;
                let expr = Expression::UnaryOperator(op, inner);
                syntax.add_expression(expr)
            },
            _ => syntax.add_expression(Expression::ExpressionParseError),
        },
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
}
