use super::common::Span;

#[derive(Debug)]
pub struct ParserError<'a> {
    pub message: String,
    pub span: Option<Span<'a>>
}

impl<'a> ParserError<'a> {
    pub fn new(message: &str, span: Span<'a>) -> Self {
        Self {
            message: message.into(),
            span: Some(span)
        }
    }
}

impl<'a> From<nom::error::Error<Span<'a>>> for ParserError<'a> {
    fn from(err: nom::error::Error<Span<'a>>) -> Self {
        let mut message = "Error: ".to_owned();
        message.push_str(err.code.description());

        Self {
            message,
            span: Some(err.input)
        }
    }
}

impl<'a> From<Box<dyn std::error::Error>> for ParserError<'a> {
    fn from(err: Box<dyn std::error::Error>) -> Self {
        Self {
            message: format!("{:#?}", err),
            span: None
        }
    }
}

impl<'a> From<nom::Err<nom::error::Error<Span<'a>>>> for ParserError<'a> {
    fn from(err: nom::Err<nom::error::Error<Span<'a>>>) -> Self {
        match err {
            nom::Err::Error(error) | nom::Err::Failure(error) => error.into(),
            nom::Err::Incomplete(_needed) => Self {
                message: format!("{}", err),
                span: None
            }
        }
    }
}

impl<'a> nom::error::ParseError<Span<'a>> for ParserError<'a> {
    fn from_error_kind(input: Span<'a>, kind: nom::error::ErrorKind) -> Self {
        nom::error::make_error::<_, nom::error::Error<Span<'a>>>(input, kind).into()
    }

    fn append(input: Span<'a>, kind: nom::error::ErrorKind, other: Self) -> Self {
        other + nom::error::make_error::<_, nom::error::Error<Span<'a>>>(input, kind).into()
    }
}

impl<'a> std::ops::Add for ParserError<'a> {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self {
            message: format!("{}, {}", self.message, rhs.message),
            span: rhs.span
        }
    }
}

impl<'a> From<(&'a str, Span<'a>)> for ParserError<'a> {
    fn from(input: (&'a str, Span<'a>)) -> Self {
        Self {
            message: input.0.to_owned(),
            span: Some(input.1)
        }
    }
}
