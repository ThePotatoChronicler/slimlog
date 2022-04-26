/// A type for the compiler, it represents the context of all compilation functions

use super::{
    ast::{
        Argument
    },
};

use std::convert::AsRef;

/// Context for all compiler functions
#[derive(Copy, Clone, Debug, Default)]
pub struct Ctx<'a, 'r> {
    pub args: &'a [Argument<'a>],
    pub ret: Option<&'r str>
}

impl<'a, 'r> Ctx<'a, 'r> {

    /// Alias for `default`
    pub fn empty() -> Self {
        Ctx::default()
    }

    /// Creates a new empty context and sets it's `ret`
    pub fn just_ret(ret: &'r str) -> Self {
        Self::empty().with_ret(ret)
    }

    /// Uses the currect context, but replaces `ret`
    pub fn with_ret(&self, ret: &'r str) -> Self {
        let mut ctx = *self;
        ctx.ret = Some(ret);
        ctx
    }

    /// Removes `ret` from currect context
    pub fn no_ret(&self) -> Self {
        let mut ctx = *self;
        ctx.ret = None;
        ctx
    }

    /// Creates an empty context and replaces it's `ret`
    pub fn from_ret(ret: &'r str) -> Self {
        Ctx::empty().with_ret(ret)
    }

    /// Uses the currect context, but replaces `args`
    pub fn with_args(&self, args: &'a [Argument]) -> Self {
        let mut ctx = *self;
        ctx.args = args;
        ctx
    }

    /// Returns the same context but with no `args`
    pub fn no_args(&self) -> Self {
        self.with_args(&[])
    }
}
