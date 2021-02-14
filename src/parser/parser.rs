use std::iter;

use super::ast::*;
use super::tokens::Token;
use super::location::Location;

#[cfg(test)]
mod test;

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
                self.next(); // eat a token to avoid infinite loops
                return self.statement_error("Statement not at expected indentation");
            }
        }

        // Only eat trailing newlines on statments that don't contain smaller
        // statements
        match token {
            Token::KeywordReturn => {
                self.next();
                let stmt = self.parse_return();
                self.eat_trailing_newline(stmt)
            },
            Token::KeywordLet => {
                self.next();
                let stmt = self.parse_let();
                self.eat_trailing_newline(stmt)
            },
            Token::KeywordIf => {
                self.next();
                self.parse_if(location.col)
            },
            Token::KeywordWhile => {
                self.next();
                self.parse_while(location.col)
            },
            Token::KeywordFor => {
                self.next();
                self.parse_for(location.col)
            },
            Token::KeywordMatch => {
                self.next();
                self.parse_match(location.col)
            },
            Token::ValueName(name) => match self.peek_next() {
                Some((Token::Equals, _)) => self.parse_assign(),
                _ => self.parse_expr_stmt(),
            },
            _ => self.parse_expr_stmt(),
        }
    }

    fn eat_trailing_newline(&mut self, result: StatementRef) -> StatementRef {
        match self.next() {
            // A statement can be the last line in a file
            None | Some((Token::Newline, _)) => result,
            Some((t, _)) => {
                self.statement_error("Unexpected token after statement")
            },
        }
    }

    fn expect_name(&mut self) -> Result<String, StatementRef> {
        match self.next() {
            Some((Token::ValueName(s), _)) => Ok(s.clone()),
            _ => Err(self.statement_error("Expected a name")),
        }
    }

    fn parse_match(&mut self, indent: u32) -> StatementRef {
        let matched = self.parse_expression();
        let matchers = match self.parse_match_block(indent) {
            Ok(m) => m,
            Err(sref) => return sref,
        };

        let match_statement = MatchStatement{matched: matched, matchers: matchers};
        let stmt = Statement::MatchStmt(Box::new(match_statement));
        self.syntax.add_statement(stmt)
    }

    fn parse_match_block(&mut self, parent_indent: u32) -> Result<Vec<Matcher>, StatementRef> {
        if !self.require_next(Token::Colon) {
            return Err(self.statement_error("expected a colon after the matched expression"));
        }

        if !self.require_next(Token::Newline) {
            return Err(self.statement_error("expected a newline after a colon"));
        }

        let mut matchers = vec![];
        let mut new_indent = None;

        // TODO: Consider making the handling of blocks generic
        loop {
            let (token, location) = match self.peek() {
                // The file could end inside a match block
                None => break,
                Some(tok) => tok,
            };

            if *token == Token::Newline {
                // ignore it
                self.next();
                continue;
            }

            // Make sure the indent is valid and the block hasn't ended
            let indent = match new_indent {
                None => {
                    if location.col <= parent_indent {
                        // The block ended with no statements
                        break;
                    }
                    new_indent = Some(location.col);
                    location.col
                },
                Some(level) => {
                    if location.col > level {
                        let err_stmt = self.statement_error("Matcher indented too far");
                        return Err(err_stmt);
                    } else if location.col < level {
                        // The block has ended
                        break;
                    }
                    level
                },
            };

            let matcher = self.parse_match_arm(indent)?;
            matchers.push(matcher);
        }

        Ok(matchers)
    }

    fn parse_match_arm(&mut self, indent: u32) -> Result<Matcher, StatementRef> {
        let location = match self.peek() {
            None => {
                return Err(self.statement_error("expected a pattern, got EOF"));
            },
            Some((_, loc)) => loc,
        };

        if location.col != indent {
            self.next(); // avoid infinite loops
            return Err(self.statement_error("pattern is at wrong level of indentation"));
        }

        let pat = self.parse_pattern();
        let block = self.parse_block(indent);
        Ok(Matcher{pattern: pat, body: block})
    }

    fn parse_pattern(&mut self) -> Pattern {
        let tok = match self.next() {
            Some((t, _)) => t,
            None => {
                self.add_error("unexpected EOF in pattern");
                return Pattern::PatternParseError;
            },
        };

        match tok {
            Token::ValueName(name) => {
                if let Some((Token::Ampersand, _)) = self.peek() {
                    self.next();
                    let pat = self.parse_pattern();
                    Pattern::Named(name.clone(), Box::new(pat))
                } else if name == "_" {
                    Pattern::Underscore
                } else {
                    Pattern::Name(name.clone())
                }
            },
            Token::TypeName(name) => self.parse_struct_pattern(name.clone()),
            Token::IntLiteral(i) => Pattern::Literal(Literal::Integer(*i)),
            Token::FloatLiteral(f) => Pattern::Literal(Literal::Float(*f)),
            Token::StringLiteral(s) => Pattern::Literal(Literal::String(s.clone())),
            Token::LParen => self.parse_tuple_pattern(),
            _ => {
                self.add_error("invalid pattern");
                Pattern::PatternParseError
            },
        }
    }

    fn parse_struct_pattern(&mut self, name: String) -> Pattern {
        let mut fields = vec![];

        if self.is_next(Token::LParen) {
            self.next();

            loop {
                match self.peek() {
                    None => break,
                    Some((Token::RParen, _)) => break,
                    _ => {},
                }

                let pat = self.parse_pattern();
                fields.push(pat);

                match self.peek() {
                    Some((Token::Comma, _)) => {
                        self.next();
                    },
                    _ => break,
                }
            }

            if !self.require_next(Token::RParen) {
                self.add_error("unclosed structure pattern");
                return Pattern::PatternParseError;
            }
        }

        let struct_pattern = StructPattern{
            struct_name: name,
            field_patterns: fields,
        };
        Pattern::Structure(Box::new(struct_pattern))
    }

    fn parse_tuple_pattern(&mut self) -> Pattern {
        let mut patterns = vec![];

        loop {
            match self.peek() {
                None => {
                    self.add_error("unclosed tuple pattern");
                    return Pattern::PatternParseError;
                },
                Some((Token::RParen, _)) => {
                    break;
                },
                _ => {
                    let pat = self.parse_pattern();
                    patterns.push(pat);

                    // The next token should be a comma or close paren
                    match self.peek() {
                        Some((Token::Comma, _)) => {
                            self.next();
                        },
                        _ => {
                            break;
                        },
                    }
                },
            }
        }

        if !self.require_next(Token::RParen) {
            self.add_error("Expected a right paren closing the tuple pattern");
            return Pattern::PatternParseError;
        }

        Pattern::Tuple(patterns)
    }

    fn parse_expr_stmt(&mut self) -> StatementRef {
        let expr = self.parse_expression();
        let stmt = self.syntax.add_statement(Statement::ExprStmt(expr));
        self.eat_trailing_newline(stmt)
    }

    fn parse_for(&mut self, indent: u32) -> StatementRef {
        let variable = match self.expect_name() {
            Ok(n) => n,
            Err(sref) => return sref,
        };
        if !self.require_next(Token::KeywordIn) {
            return self.statement_error("expected 'in' after the for loop's variable");
        }
        let iterable = self.parse_expression();
        let body = self.parse_block(indent);
        let for_statement = ForStatement{variable: variable, iterable: iterable, body: body};
        let stmt = Statement::ForStmt(Box::new(for_statement));
        self.syntax.add_statement(stmt)
    }

    fn parse_while(&mut self, indent: u32) -> StatementRef {
        let test = self.parse_expression();
        let body = self.parse_block(indent);
        let while_statement = WhileStatement{test: test, body: body};
        let stmt = Statement::WhileStmt(Box::new(while_statement));
        self.syntax.add_statement(stmt)
    }

    fn parse_if(&mut self, indent: u32) -> StatementRef {
        let test = self.parse_expression();
        let tbody = self.parse_block(indent);

        let ebody = if let Some((Token::KeywordElse, location)) = self.peek() {
            self.next();
            if location.col != indent {
                return self.statement_error("'else' not at expected indentation");
            }

            Some(self.parse_block(indent))
        } else {
            None
        };

        let if_statement = IfStatement{test: test, tbody: tbody, ebody: ebody};
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

    fn parse_assign(&mut self) -> StatementRef {
        let name = match self.next() {
            Some((Token::ValueName(n), _)) => n.clone(),
            _ => {
                return self.statement_error("Expected a name for an assignment");
            },
        };

        if !self.require_next(Token::Equals) {
            return self.statement_error("Expected an = after let <name>");
        }

        let expr = self.parse_expression();
        let stmt = Statement::AssignStmt(name, expr);
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

    fn is_done(&self) -> bool {
        self.peek().is_none()
    }

    fn peek(&self) -> Option<&'a (Token, Location)> {
        self.tokens.get(self.index)
    }

    fn peek_next(&self) -> Option<&'a (Token, Location)> {
        self.tokens.get(self.index + 1)
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

    fn add_error(&mut self, message: &str) {
        self.errors.push((self.index, message.to_string()));
    }

    fn statement_error(&mut self, message: &str) -> StatementRef {
        self.add_error(message);
        self.syntax.add_statement(Statement::StatementParseError)
    }

    fn expression_error(&mut self, message: &str) -> ExpressionRef {
        self.add_error(message);
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

    fn show_error(&self, mut index: usize, message: &String) -> String {
        index = index.min(self.tokens.len() - 1);
        let location = if self.tokens.is_empty() {
            "start".to_string()
        } else {
            self.tokens[index].1.to_string()
        };
        format!(
            "error: {} at around {}. Tokens: {}",
            message, location, self.preview(index)
        )
    }

    fn show_errors(&self) -> Vec<String> {
        self.errors.iter().map(|(idx, message)| self.show_error(*idx, message)).collect()
    }
}
