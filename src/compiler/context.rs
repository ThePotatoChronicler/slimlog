/// A type for the compiler, it represents the context of all compilation functions

use super::{
    ast::{
        Argument
    },
    instructions::Vartype,
};

use std::cell::Cell;

/// Holds the data necessary to create a [`Ctx`]
#[derive(Debug)]
pub struct Context {
    seq: Cell<usize>
}

/// Context for all compiler functions
#[derive(Copy, Clone, Debug)]
pub struct Ctx<'a, 'r, 's> {
    /// Arguments
    pub args: &'a [Argument<'a>],
    /// Return
    pub ret: Option<&'r Vartype>,
    /// Sequence of numbers
    seq: &'s Cell<usize>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            seq: Cell::new(0)
        }
    }

    /// Creates an empty child Ctx
    pub fn create_empty_ctx(&self) -> Ctx {
        Ctx {
            args: &[],
            ret: None,
            seq: &self.seq
        }
    }
}

impl<'a, 'r, 's> Ctx<'a, 'r, 's> {

    /// Uses the currect context, but replaces `ret`
    pub fn with_ret(&self, ret: &'r Vartype) -> Self {
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

    pub fn get_next(&self) -> usize {
        let current = self.seq.get();
        self.seq.set(current + 1);
        current
    }
}
