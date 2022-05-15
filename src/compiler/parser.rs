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
    Slice,
};

use super::{
    error::ParserError,
    common::Span,
    ast
};

type IResult<'a, 'b> = nom::IResult<Span<'a>, Span<'b>>;

/// Automatically converts input to a Span before passing it off to inner_tokenize
pub fn tokenize(input: &str) -> Result<ast::Statement, ParserError>{
    inner_tokenize(Span::new(input))
}

pub fn inner_tokenize(input: Span) -> Result<ast::Statement, ParserError> {
    let (r, _) = multispace0::<_, nom::error::Error<_>>(input).unwrap();
    let (remains, stmnt) = match statement(r) {
        Ok(res) => res,
        Err(stmnt_err) => match stmnt_err {
            StatementError::ParseArg(err) => return Err(err),
            StatementError::NotAStatement => return Err(ParserError::from((
                "Program must begin with a statement",
                r,
            ))),
            other => return Err(common_parse_some_statement_error(other, input)),
        }
    };
    let (remains, _) = multispace0::<_, nom::error::Error<_>>(remains).unwrap();

    if !remains.is_empty() {
        Err(ParserError::from(("Extra characters after main statement", remains)))
    } else {
        Ok(stmnt)
    }
}

pub fn identifier(input: Span) -> IResult {
    recognize(
        pair(
            is_a(
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/|&=_<>@#"),
            many0_count(is_a(
                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-*/|&=_<>@#0123456789"))
        )
    )(input)
}

pub fn decimal(input: Span) -> IResult {
    is_a("0123456789")(input)
}

#[derive(Debug)]
pub enum StringError {
    NotAString,
    Unmatched,
}

pub fn string(input: Span) -> Result<(Span, Span), StringError> {

    // Gets one of the two characters, or errors out
    let (_, start) = one_of::<_, _, nom::error::Error<_>>(r#""'"#)(input)
        .map_err(|_| StringError::NotAString)?;

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
            None => return Err(StringError::Unmatched)
        }
    }
}

#[derive(Debug)]
pub enum BalanceError {
    /// Missing opening
    Start,
    /// Missing endings
    End(u32)
}

pub fn take_until_balanced (opening: char, txt: Span, closing: char) -> Result<(Span, Span), BalanceError> {
    let (i, _) = char::<_, nom::error::Error<_>>(opening)(txt)
        .map_err(|_| BalanceError::Start)?;
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
            return Err(BalanceError::End(depth))
        }

        if depth == 0 {
            break Ok((i.slice(index + 1..), i.slice(..index)))
        }

        index += 1;
    }
}

pub enum StatementError<'a> {
    /// This is not a statement
    NotAStatement,
    /// Invalid identifier for command
    InvalidIdent(Span<'a>),
    /// Unclosed statement
    Unclosed(u32),
    /// Failed to parse argument
    ParseArg(ParserError<'a>)
}

pub fn statement(input: Span) -> Result<(Span, ast::Statement), StatementError> {
    let (statement_remains, stmnt_blk) = match take_until_balanced('(', input, ')') {
        Ok(res) => res,
        Err(err) => match err {
            BalanceError::Start => return Err(StatementError::NotAStatement),
            BalanceError::End(brackets) => return Err(StatementError::Unclosed(brackets)),
        }
    };

    let (r, _) = multispace0::<_, nom::error::Error<_>>(stmnt_blk).unwrap();
    let (r, command) = identifier(r)
        .map_err(|_| StatementError::InvalidIdent(r))?;
    let mut arguments: Vec<ast::Argument> = Vec::new();
    let mut inr = r;
    loop {
        (inr, _) = multispace0::<_, nom::error::Error<_>>(inr).unwrap();
        if inr.is_empty() {
            break
        }
        let (remains, arg): (Span, ast::Argument) = parse_arg(inr)
            .map_err(|e| StatementError::ParseArg(e))?;

        inr = remains;

        arguments.push(arg);
    }
    Ok((statement_remains, ast::Statement { command, arguments }))
}

fn parse_arg(input: Span) -> Result<(Span, ast::Argument), ParserError> {
    use ast::Argument;

    if let Ok((r, ident)) = identifier(input) {
        return Ok((r, Argument::Identifier(ident)));
    }

    if let Ok((r, dcml)) = decimal(input) {
        return Ok((r, Argument::Number(dcml)));
    }

    match string(input) {
        Ok((r, parsed_string)) => return Ok((r, Argument::String(parsed_string))),
        Err(StringError::NotAString) => (),
        Err(StringError::Unmatched) => return Err(ParserError::from(("Unmatched string delimiter", input))),
    }

    match statement(input) {
        Ok((r, stmnt)) => return Ok((r, Argument::Statement(stmnt))),
        Err(err) => match err {
            StatementError::NotAStatement => (),
            StatementError::ParseArg(err) => return Err(err),
            other => return Err(common_parse_some_statement_error(other, input)),
        }
    }

    Err(ParserError::from(("Unparsable argument", input)))
}

fn common_parse_some_statement_error<'s>(err: StatementError<'s>, span: Span<'s>) -> ParserError<'s> {
    match err {
        StatementError::InvalidIdent(identspan) => ParserError::from((
            "Statement must begin with a valid identifier",
            identspan
        )),
        StatementError::Unclosed(brackets) => {
            let s = if brackets == 1 { "" } else { "s" };
            ParserError::from((
                format!("Unclosed statement, {brackets} bracket{s} left open"),
                span
            ))
        },
        _ => unimplemented!("Other errors should not be parsed with this function"),
    }
}
