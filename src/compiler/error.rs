use super::common::Span;
use std::{
    fmt::Display,
};

#[derive(Debug)]
pub struct ParserError<'a> {
    pub message: String,
    pub span: Span<'a>
}

impl<'a> Display for ParserError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}:{} {}", self.span.location_line(), self.span.get_column(), self.message)
    }
}

impl<'a> From<(&'_ str, Span<'a>)> for ParserError<'a> {
    fn from((message, span): (&'_ str, Span<'a>)) -> Self {
        Self::from((message.to_owned(), span))
    }
}

impl<'a> From<(String, Span<'a>)> for ParserError<'a> {
    fn from((message, span): (String, Span<'a>)) -> Self {
        Self {
            message,
            span,
        }
    }
}
