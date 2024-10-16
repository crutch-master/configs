use regex::Regex;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LiteralValue {
    String(String),
    Number(i64),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Token {
    Comment(String),
    Set,
    ArrayBegin,
    ArrayEnd,
    DictBegin,
    DictSep,
    DictEnd,
    Identifier(String),
    ExprBegin,
    ExprEnd,
    Delimiter,
    Literal(LiteralValue),
}

static SIMPLE_TOKENS: [(&'static str, Token); 9] = [
    ("set", Token::Set),
    ("array(", Token::ArrayBegin),
    (")", Token::ArrayEnd),
    ("{", Token::DictBegin),
    (":", Token::DictSep),
    ("}", Token::DictEnd),
    ("?[", Token::ExprBegin),
    ("]", Token::ExprEnd),
    (",", Token::Delimiter),
];

fn lex_simple(text: &str) -> Option<(Token, &str)> {
    let (str, token) = SIMPLE_TOKENS
        .iter()
        .find(|token| text.starts_with(token.0))?;

    return Some((token.clone(), &text[str.len()..]));
}

fn lex_comment(text: &str) -> Option<(Token, &str)> {
    if !text.starts_with("#=") {
        return None;
    }

    let end = text.find("=#")?;
    let comment = &text[2..end];

    Some((Token::Comment(comment.into()), &text[end + 2..]))
}

fn lex_number(text: &str) -> Option<(Token, &str)> {
    let digits: String = text
        .chars()
        .take_while(|chr| char::is_numeric(*chr))
        .collect();

    if digits.len() == 0 {
        return None;
    }

    Some((
        Token::Literal(LiteralValue::Number(digits.parse().ok()?)),
        &text[digits.len()..],
    ))
}

fn lex_string(text: &str) -> Option<(Token, &str)> {
    if text.chars().next()? != '"' {
        return None;
    }

    let end = text[1..].find('"')? + 1;

    return Some((
        Token::Literal(LiteralValue::String(text[1..end].into())),
        &text[end + 1..],
    ));
}

fn lex_identifier(text: &str) -> Option<(Token, &str)> {
    let regex = Regex::new("^[a-zA-Z][_a-zA-Z0-9]*").unwrap();
    let identifier = regex.find(text)?;

    return Some((
        Token::Identifier(text[..identifier.end()].into()),
        &text[identifier.end()..],
    ));
}

pub fn lex(text: &str) -> Option<Vec<Token>> {
    let mut result: Vec<Token> = Vec::new();
    let mut text = text;

    loop {
        text = text.trim_start();
        let token = None
            .or_else(|| lex_simple(text))
            .or_else(|| lex_comment(text))
            .or_else(|| lex_number(text))
            .or_else(|| lex_string(text));

        if let Some((token, rest)) = token {
            text = rest;
            result.push(token);
            continue;
        }

        break;
    }

    if text != "" {
        return None;
    }

    return Some(result);
}
