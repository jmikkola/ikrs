use std::iter;

use super::ast::*;
use super::tokens::Token;
use super::location::Location;

pub fn parse<'a>(tokens: &'a Vec<(Token, Location)>) -> Syntax {
    Parser::new(tokens).parse()
}

struct Parser<'a> {
    tokens: &'a Vec<(Token, Location)>,
    index: usize,
    syntax: Syntax,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a Vec<(Token, Location)>) -> Self {
        Parser{
            tokens: tokens,
            index: 0,
            syntax: Syntax::new(),
        }
    }

    fn parse(self) -> Syntax {
        // TODO
        self.syntax
    }

    // Parse binary operators (e.g. 1 + 2 - 3)
    // TODO: Handle the operators ^ & | ** << >>
    fn parse_expression(&mut self) -> ExpressionRef {
        // Gather terms and operators
        let mut unary_exprs = vec![];
        let mut operators: Vec<(BinaryOp, u16)> = vec![];

        unary_exprs.push(self.parse_unary());

        loop {
            let token = match self.peek() {
                None => break,
                Some((t, _)) => t,
            };

            let (op, precedence) = match token {
                Token::DoubleOr      => (BinaryOp::BoolOr, 0),
                Token::DoubleAnd     => (BinaryOp::BoolAnd, 1),
                Token::DoubleEquals  => (BinaryOp::Equal, 2),
                Token::NotEquals     => (BinaryOp::NotEqual, 2),
                Token::Less          => (BinaryOp::Less, 3),
                Token::LessEquals    => (BinaryOp::LessEqual, 3),
                Token::Greater       => (BinaryOp::Greater, 3),
                Token::GreaterEquals => (BinaryOp::GreaterEqual, 3),
                Token::Plus          => (BinaryOp::Plus, 4),
                Token::Minus         => (BinaryOp::Minus, 4),
                Token::Star          => (BinaryOp::Times, 5),
                Token::Slash         => (BinaryOp::Divide, 5),
                Token::Percent       => (BinaryOp::Mod, 5),
                // Reached the end of the binary expressions
                _ => break,
            };
            self.next();

            operators.push((op, precedence));
            unary_exprs.push(self.parse_unary());
        }

        // Group expression by precedence
        let max_prec: u16 = 5;
        // Do one pass for each level of precedence, grouping just items at that
        // level of precedence
        for p in 0..=max_prec {
            let precedence = max_prec - p;

            debug_assert!(
                unary_exprs.len() == 1 + operators.len(),
                "have {:?} unary exprs and {:?} operators",
                unary_exprs, operators
            );

            let mut remaining_exprs: Vec<ExpressionRef> = vec![unary_exprs[0]];
            let mut remaining_operators: Vec<(BinaryOp, u16)> = vec![];

            // Loop through the current expressions and operators, looking for
            // things to group
            for (i, (op, prec)) in operators.iter().enumerate() {
                if *prec == precedence {
                    let left = remaining_exprs.pop().unwrap();
                    let right = unary_exprs[i + 1];
                    let expr = Expression::BinaryOperator(*op, left, right);
                    remaining_exprs.push(self.syntax.add_expression(expr));
                } else {
                    remaining_exprs.push(unary_exprs[i + 1]);
                    remaining_operators.push((*op, *prec));
                }
            }

            unary_exprs = remaining_exprs;
            operators = remaining_operators;
        }

        debug_assert!(unary_exprs.len() == 1);
        debug_assert!(operators.len() == 0);
        *unary_exprs.first().unwrap()
    }

    // Unary operators and the value they apply to
    fn parse_unary(&mut self) -> ExpressionRef {
        let token = match self.peek() {
            None => {
                return self.syntax.add_expression(Expression::ExpressionParseError)
            },
            Some((t, _)) => t,
        };

        match token {
            Token::Bang => {
                self.next();
                let inner = self.parse_unary();
                let op = UnaryOp::BoolNot;
                let expr = Expression::UnaryOperator(op, inner);
                self.syntax.add_expression(expr)
            },
            Token::Tilda => {
                self.next();
                let inner = self.parse_unary();
                let op = UnaryOp::BitInvert;
                let expr = Expression::UnaryOperator(op, inner);
                self.syntax.add_expression(expr)
            },
            Token::Minus => {
                self.next();
                let inner = self.parse_unary();
                let op = UnaryOp::Negate;
                let expr = Expression::UnaryOperator(op, inner);
                self.syntax.add_expression(expr)
            },
            _ => {
                self.parse_term()
            },
        }
    }

    // Single values and field access, array access, and function calls
    fn parse_term(&mut self) -> ExpressionRef {
        let mut value = self.parse_single_value();

        // This loop handles zero or more field access, offet access, and/or
        // function arguments after an expression.
        // E.g. if the expression is `x.foo[1].bar()`, then `x` is the current value
        // and each time though the loop picks up another one of `.foo`, `[1]`,
        // `.bar`, and `()`.
        loop {
            value = match self.peek() {
                Some((Token::LParen, _)) => {
                    self.next();
                    let mut args = vec![];

                    // Read args passed to function, allowing a trailing comma
                    while !self.is_next(Token::RParen) {
                        args.push(self.parse_expression());
                        if self.is_next(Token::Comma) {
                            self.next();
                        } else {
                            break;
                        }
                    }

                    if self.require_next(Token::RParen) {
                        self.syntax.add_expression(Expression::FunctionCall(value, args))
                    } else {
                        self.syntax.add_expression(Expression::ExpressionParseError)
                    }
                },
                Some((Token::LBracket, _)) => {
                    self.next();
                    let offset = self.parse_expression();

                    if self.require_next(Token::RBracket) {
                        self.syntax.add_expression(Expression::OffsetAccess(value, offset))
                    } else {
                        self.syntax.add_expression(Expression::ExpressionParseError)
                    }
                },
                Some((Token::Dot, _)) => {
                    self.next();
                    // expecting a field name
                    match self.next() {
                        Some((Token::ValueName(s), _)) => {
                            self.syntax.add_expression(Expression::FieldAccess(value, s.clone()))
                        },
                        _ => {
                            self.syntax.add_expression(Expression::ExpressionParseError)
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
    fn parse_single_value(&mut self) -> ExpressionRef {
        let token = match self.next() {
            None => {
                return self.syntax.add_expression(Expression::ExpressionParseError)
            },
            Some((t, _)) => t,
        };

        match token {
            Token::LParen => {
                let inner = self.parse_expression();
                if self.require_next(Token::RParen) {
                    self.syntax.add_expression(Expression::Paren(inner))
                } else {
                    self.syntax.add_expression(Expression::ExpressionParseError)
                }
            },
            Token::IntLiteral(i) => {
                let expr = Expression::Literal(Literal::Integer(*i));
                self.syntax.add_expression(expr)
            },
            Token::FloatLiteral(f) => {
                let expr = Expression::Literal(Literal::Float(*f));
                self.syntax.add_expression(expr)
            },
            Token::StringLiteral(s) => {
                let expr = Expression::Literal(Literal::String(s.clone()));
                self.syntax.add_expression(expr)
            },
            Token::ValueName(s) => {
                let expr = Expression::Variable(s.clone());
                self.syntax.add_expression(expr)
            },
            _ => self.syntax.add_expression(Expression::ExpressionParseError),
        }
    }

    fn next(&mut self) -> Option<&'a (Token, Location)> {
        let result = self.peek();
        if result.is_some() {
            self.index += 1;
        }
        result
    }

    fn peek(&self) -> Option<&'a (Token, Location)> {
        self.tokens.get(self.index)
    }

    fn is_next(&self, expected: Token) -> bool {
        match self.peek() {
            None => false,
            Some((token, _)) => *token == expected,
        }
    }

    fn require_next(&mut self, expected: Token) -> bool {
        match self.next() {
            None => false,
            Some((token, _)) => *token == expected,
        }
    }
}

/*
Ideas:

- enumerate the tokens, allowing errors to contain the token offset
- add a location and explaination enum to StatementParseError
- add an error utility to show the tokens starting from the error
*/

fn parse_statement<'a, I>(
    cursor: &mut iter::Peekable<I>,
    syntax: &mut Syntax,
    indent: Option<u32>,
) -> StatementRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let (token, location) = match cursor.peek() {
        None => {
            return syntax.add_statement(Statement::StatementParseError);
        },
        Some(tok) => tok,
    };

    if let Some(level) = indent {
        if location.col != level {
            return syntax.add_statement(Statement::StatementParseError);
        }
    }

    let result = match token {
        Token::KeywordReturn => {
            cursor.next();
            parse_return(cursor, syntax)
        },
        Token::KeywordLet => {
            cursor.next();
            parse_let(cursor, syntax)
        },
        Token::KeywordIf => {
            cursor.next();
            parse_if(cursor, syntax, location.col)
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

    match cursor.next() {
        // A statement can be the last line in a file
        None | Some((Token::Newline, _)) => result,
        _ => syntax.add_statement(Statement::StatementParseError),
    }
}

fn parse_if<'a, I>(
    cursor: &mut iter::Peekable<I>,
    syntax: &mut Syntax,
    indent: u32,
) -> StatementRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let test = parse_expression(cursor, syntax);
    let tbody = parse_block(cursor, syntax, indent);

    // TODO: Else statement

    let if_statement = IfStatement{test: test, tbody: tbody, ebody: None};
    let stmt = Statement::IfStmt(Box::new(if_statement));
    syntax.add_statement(stmt)
}

// returns the block statement
fn parse_block<'a, I>(
    cursor: &mut iter::Peekable<I>,
    syntax: &mut Syntax,
    indent: u32,
) -> StatementRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    if !require_next(Token::Colon, cursor) {
        return syntax.add_statement(Statement::StatementParseError);
    }

    if !require_next(Token::Newline, cursor) {
        return syntax.add_statement(Statement::StatementParseError);
    }

    let mut statements = vec![];
    let mut new_indent = None;

    loop {
        let (token, location) = match cursor.peek() {
            // The file could end inside a block
            None => break,
            Some(tok) => tok,
        };

        if *token == Token::Newline {
            // ignore it
            cursor.next();
            continue;
        }

        // Make sure the indent is valid and the block hasn't ended
        match new_indent {
            None => {
                if location.col <= indent {
                    // The block ended with no statements
                    break;
                }
                new_indent = Some(location.col);
            },
            Some(level) => {
                if location.col > level {
                    // Invalid indentation -- parse the statement anyway to
                    // leave the stream of tokens sensible
                    parse_statement(cursor, syntax, None);
                    let err_stmt = syntax.add_statement(Statement::StatementParseError);
                    statements.push(err_stmt);
                    break;
                } else if location.col <= level {
                    // The block has ended
                    break;
                }
            },
        }

        let stmt = parse_statement(cursor, syntax, new_indent);
        statements.push(stmt);
    }

    let stmt = Statement::Block(statements);
    syntax.add_statement(stmt)
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

fn parse_let<'a, I>(cursor: &mut iter::Peekable<I>, syntax: &mut Syntax) -> StatementRef
where I: iter::Iterator<Item=&'a (Token, Location)> {
    let name = match cursor.next() {
        Some((Token::ValueName(n), _)) => n.clone(),
        _ => {
            return syntax.add_statement(Statement::StatementParseError);
        },
    };

    if !require_next(Token::Equals, cursor) {
        return syntax.add_statement(Statement::StatementParseError);
    }

    let expr = parse_expression(cursor, syntax);
    let stmt = Statement::LetStmt(name, expr);
    syntax.add_statement(stmt)
}



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
        let tokens = tokenize(input);
        let mut parser = Parser::new(&tokens);
        let eref = parser.parse_expression();
        let s = parser.syntax;
        let inspected = inspect(eref, &s).unwrap();
        assert_eq!(expected, inspected.as_str());
    }

    fn assert_parses_stmt(input: &str, expected: &str) {
        let mut s = Syntax::new();
        let sref = parse_statement(
            &mut tokenize(input).iter().peekable(),
            &mut s,
            None);
        let inspected = inspect(sref, &s).unwrap();
        assert_eq!(expected, inspected.as_str());
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
        let stmt = "if 1:\n  return\n  return 123\n";
        println!("tokens: {:?}", tokenize(stmt));
        assert_parses_stmt(stmt, "(if 1 (do (return) (return 123)))");
    }
}
