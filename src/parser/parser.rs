use std::iter;

use super::ast::*;
use super::tokens::Token;
use super::location::Location;

pub fn parse<'a>(tokens: &'a Vec<(Token, Location)>) -> Syntax {
    Parser::new(tokens).parse()
}


/*
Ideas:

- enumerate the tokens, allowing errors to contain the token offset
- add a location and explaination enum to StatementParseError
- add an error utility to show the tokens starting from the error
 */

struct Parser<'a> {
    tokens: &'a Vec<(Token, Location)>,
    index: usize,
    syntax: Syntax,

    // TODO: Find a better format for errors
    // (index, error message)
    errors: Vec<(usize, String)>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a Vec<(Token, Location)>) -> Self {
        Parser{
            tokens: tokens,
            index: 0,
            syntax: Syntax::new(),
            errors: Vec::new(),
        }
    }

    fn parse(self) -> Syntax {
        // TODO
        self.syntax
    }

    fn parse_statement(&mut self, indent: Option<u32>) -> StatementRef {
        let (token, location) = match self.peek() {
            None => {
                return self.statement_error("Expected a statement, got EOF");
            },
            Some(tok) => tok,
        };

        if let Some(level) = indent {
            if location.col != level {
                return self.statement_error("Statement not at expected indentation");
            }
        }

        let result = match token {
            Token::KeywordReturn => {
                self.next();
                self.parse_return()
            },
            Token::KeywordLet => {
                self.next();
                self.parse_let()
            },
            Token::KeywordIf => {
                self.next();
                self.parse_if(location.col)
            },
            Token::KeywordWhile => {
                self.next();
                // TODO: While statement
                self.statement_error("TODO")
            },
            Token::KeywordFor => {
                self.next();
                // TODO: For statement
                self.statement_error("TODO")
            },
            Token::KeywordMatch => {
                self.next();
                // TODO: Match statement
                self.statement_error("TODO")
            },
            Token::ValueName(name) => {
                // TODO: Parse assignment or expression
                self.statement_error("TODO")
            },
            _ => {
                let expr = self.parse_expression();
                let stmt = Statement::ExprStmt(expr);
                self.syntax.add_statement(stmt)
            },
        };

        match self.next() {
            // A statement can be the last line in a file
            None | Some((Token::Newline, _)) => result,
            Some((t, _)) => {
                self.statement_error("Unexpected token after statement")
            },
        }
    }

    fn parse_if(&mut self, indent: u32) -> StatementRef {
        let test = self.parse_expression();
        let tbody = self.parse_block(indent);

        // TODO: Else statement

        let if_statement = IfStatement{test: test, tbody: tbody, ebody: None};
        let stmt = Statement::IfStmt(Box::new(if_statement));
        self.syntax.add_statement(stmt)
    }

    // returns the block statement
    fn parse_block(&mut self, indent: u32) -> StatementRef {
        if !self.require_next(Token::Colon) {
            return self.statement_error("Expected a block to start with a colon");
        }

        if !self.require_next(Token::Newline) {
            return self.statement_error("Expected a newline after a colon");
        }

        let mut statements = vec![];
        let mut new_indent = None;

        loop {
            let (token, location) = match self.peek() {
                // The file could end inside a block
                None => break,
                Some(tok) => tok,
            };

            if *token == Token::Newline {
                // ignore it
                self.next();
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
                        let err_stmt = self.statement_error("Statement too far indented");
                        // Invalid indentation -- parse the statement anyway to
                        // leave the stream of tokens sensible
                        self.parse_statement(None);
                        statements.push(err_stmt);
                        break;
                    } else if location.col < level {
                        // The block has ended
                        break;
                    }
                },
            }

            let stmt = self.parse_statement(new_indent);
            statements.push(stmt);
        }

        let stmt = Statement::Block(statements);
        self.syntax.add_statement(stmt)
    }

    fn parse_return(&mut self) -> StatementRef {
        let token = match self.peek() {
            None => {
                return self.syntax.add_statement(Statement::Return);
            },
            Some((t, _)) => t,
        };

        if *token == Token::Newline {
            // The caller eats this newline
            self.syntax.add_statement(Statement::Return)
        } else {
            let expr = self.parse_expression();
            self.syntax.add_statement(Statement::ReturnExpr(expr))
        }
    }

    fn parse_let(&mut self) -> StatementRef {
        let name = match self.next() {
            Some((Token::ValueName(n), _)) => n.clone(),
            _ => {
                return self.statement_error("Expected a name after let");
            },
        };

        if !self.require_next(Token::Equals) {
            return self.statement_error("Expected an = after let <name>");
        }

        let expr = self.parse_expression();
        let stmt = Statement::LetStmt(name, expr);
        self.syntax.add_statement(stmt)
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
                return self.expression_error("Expected an expression, got EOF")
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
                        self.expression_error("Expected a )")
                    }
                },
                Some((Token::LBracket, _)) => {
                    self.next();
                    let offset = self.parse_expression();

                    if self.require_next(Token::RBracket) {
                        self.syntax.add_expression(Expression::OffsetAccess(value, offset))
                    } else {
                        self.expression_error("Expected a ]")
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
                            self.expression_error("Expected a field name after a dot")
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
                return self.expression_error("Expected a value, got EOF")
            },
            Some((t, _)) => t,
        };

        match token {
            Token::LParen => {
                let inner = self.parse_expression();
                if self.require_next(Token::RParen) {
                    self.syntax.add_expression(Expression::Paren(inner))
                } else {
                    self.expression_error("Expected a )")
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
            _ => self.expression_error("Unexpected token for a value"),
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

    fn statement_error(&mut self, message: &str) -> StatementRef {
        self.errors.push((self.index, message.to_string()));
        self.syntax.add_statement(Statement::StatementParseError)
    }

    fn expression_error(&mut self, message: &str) -> ExpressionRef {
        self.errors.push((self.index, message.to_string()));
        self.syntax.add_expression(Expression::ExpressionParseError)
    }

    fn preview(&self, index: usize) -> String {
        // Back up one token, because often the relevant token has already been
        // read
        let start = if index > 0 { index - 1 } else { index };
        self.tokens.iter().skip(start).take(5)
            .map(|(tok, _)| format!("{}", tok))
            .collect::<Vec<String>>()
            .join(" ")
    }

    fn show_error(&self, index: usize, message: &String) -> String {
        format!("error: {} at around {}. Tokens: {}",
                message,
                self.tokens[index].1.to_string(),
                self.preview(index))
    }

    fn show_errors(&self) -> Vec<String> {
        self.errors.iter().map(|(idx, message)| self.show_error(*idx, message)).collect()
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
}
