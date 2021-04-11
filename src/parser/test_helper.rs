use super::ast;
use super::tokenize::tokenize;
use super::parser::parse;

pub fn tokenize_and_parse(text: &str) -> Option<ast::Syntax> {
    let tokens = tokenize(text);
    if tokens.has_unknown() {
        return None;
    }

    let syntax = parse(&tokens);
    if syntax.has_errors() {
        return None;
    }

    Some(syntax)
}
