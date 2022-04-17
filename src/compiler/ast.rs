use super::common::Span;

#[derive(Debug)]
pub struct Statement<'a> {
    pub command: Span<'a>,
    pub arguments: Vec<Argument<'a>>
}

#[derive(Debug)]
pub enum Argument<'a> {
    Identifier(Span<'a>),

    /// Both integers and floats
    Number(Span<'a>),
    String(Span<'a>),
    Statement(Statement<'a>)
}

impl<'a> Argument<'a> {
    pub fn is_statement(&self) -> bool {
        matches!(self, Argument::Statement(_))
    }
}
