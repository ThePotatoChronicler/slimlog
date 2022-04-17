use nom_locate::LocatedSpan;
use super::error::MlogsError;

pub type Span<'a> = LocatedSpan<&'a str>;
pub type MResult<'a, I, O> = Result<(I, O), MlogsError<'a>>;
