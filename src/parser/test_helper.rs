use super::ast;
use super::parser::parse;
use super::tokenize::tokenize;

#[allow(dead_code)]
pub fn tokenize_and_parse(text: &str) -> Option<ast::Syntax> {
    tokenize_and_parse_with_name(text, "test")
}

pub fn tokenize_and_parse_with_name(text: &str, filename: &str) -> Option<ast::Syntax> {
    let to_trim: &[_] = &[' ', '\n'];
    let trimmed = text.trim_start_matches(to_trim);

    let tokens = tokenize(trimmed);
    if tokens.has_unknown() {
        return None;
    }

    let syntax = parse(filename, &tokens);
    if syntax.has_errors() {
        return None;
    }

    Some(syntax)
}
