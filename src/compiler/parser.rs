use nom::{
    character::{
        complete::{
            char,
            multispace0,
            one_of
        },
    },
    sequence::pair,
    bytes::complete::is_a,
    multi::many0_count,
    combinator::recognize,
    Needed,
    Slice
};

use super::{
    error::ParserError,
    common::Span,
    ast
};

pub type ParserResult<'a, I, O> = Result<(I, O), ParserError<'a>>;

/// Automatically converts input to a Span before passing it off to inner_tokenize
pub fn tokenize(input: &str) -> Result<ast::Statement, ParserError>{
    inner_tokenize(Span::new(input))
}

pub fn inner_tokenize(input: Span) -> Result<ast::Statement, ParserError> {
    let (r, _) = multispace0(input)?;
    let (remains, b) = take_until_balanced('(', r, ')')?;

    let (remains, _) = multispace0(remains)?;
    if !remains.is_empty() {
        return Err(ParserError::new("Extra characters after main block", remains))
    }

    let (r, stmnt) = statement(b)?;
    if !r.is_empty() {
        Err(ParserError::new("Extra characters in main block", r))
    } else {
        Ok(stmnt)
    }
}

pub fn identifier(input: Span) -> ParserResult<Span, Span> {
    Ok(recognize(
        pair(
            is_a(
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/|&=_<>@#"),
            many0_count(is_a(
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/|&=_<>@#0123456789"))
        )
    )(input)?)
}

pub fn decimal(input: Span) -> ParserResult<Span, Span> {
    Ok(is_a("0123456789")(input)?)
}

pub fn string(input: Span) -> ParserResult<Span, Span> {

    // Gets one of the two characters, or errors out
    let (_, start) = one_of("\"'")(input)?;

    let mut esc: bool = false;
    let mut index = 1;
    loop {
        match input.chars().nth(index) {
            Some(c) => {
                if esc {
                    esc = false;
                } else if c == start {
                    return Ok((input.slice(index + 1..), input.slice(..index + 1)))
                } else if c == '\\' {
                    esc = true;
                }
                index += 1;
            },
            None => return Err(ParserError::new("Unmatched quote", input))
        }
    }
}

pub fn take_until_balanced (opening: char, txt: Span, closing: char) -> ParserResult<Span, Span> {
    let (i, _) = char(opening)(txt)?;
    let mut index = 0;
    let mut depth = 1;
    loop {
        let next: Option<char> = i.chars().nth(index);
        if let Some(c) = next {
            match c {
                _ if c == opening => depth += 1,
                _ if c == closing => depth -= 1,
                _ => ()
            };
        } else {
            Err(nom::Err::Incomplete::<nom::error::Error<Span>>(Needed::Unknown))?
        }

        if depth == 0 {
            break Ok((i.slice(index + 1..), i.slice(..index)))
        }

        index += 1;
    }
}

pub fn statement(input: Span) -> ParserResult<Span, ast::Statement> {
    let (r, _) = multispace0(input)?;
    let (r, command) = identifier(r)?;
    let mut arguments: Vec<ast::Argument> = Vec::new();
    let mut inr = r;
    loop {
        (inr, _) = multispace0(inr)?;
        if inr.is_empty() {
            break
        }
        let arg: ast::Argument = {
            if let Ok((r, idnt)) = identifier(inr) {
                inr = r;
                ast::Argument::Identifier(idnt)
            } else if let Ok((r, dcml)) = decimal(inr) {
                inr = r;
                ast::Argument::Number(dcml)
            } else if let Ok((r, strng)) = string(inr) {
                inr = r;
                ast::Argument::String(strng)
            } else if let Ok((r, blk)) = take_until_balanced('(', inr, ')') {
                inr = r;
                let (b, _) = multispace0(blk)?;
                let (remains, stmnt) = statement(b)?;
                let (remains, _) = multispace0(remains)?;
                if !remains.is_empty() {
                    return Err(ParserError::new("Extra characters in statement", remains))
                } else {
                    ast::Argument::Statement(stmnt)
                }
            } else {
                return Err(ParserError::new("Not a valid argument", inr))
            }
        };
        arguments.push(arg);
    }
    let r = inr;
    Ok((r, ast::Statement { command, arguments }))
}

#[cfg(test)]
mod tests {
}
