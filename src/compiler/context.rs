/// A type for the compiler, it represents the context of all compilation functions

use super::{
    ast::{
        Argument
    },
    instructions::Vartype,
};

use std::{
    cell::{ Cell, RefCell },
    collections::HashSet,
    rc::Rc,
};

/// Holds the data necessary to create a [`Ctx`]
#[derive(Debug)]
pub struct Context {
    /// Sequences of numbers starting from 0
    seq: Cell<usize>,
    /// String register
    namereg: RefCell<HashSet<Rc<str>>>,
}

/// Context for all compiler functions
#[derive(Copy, Clone, Debug)]
pub struct Ctx<'a, 'r, 'c> {
    /// Arguments
    pub args: &'a [Argument<'a>],
    /// Return
    pub ret: Option<&'r Vartype>,
    /// Sequence of numbers
    seq: &'c Cell<usize>,
    /// String storage
    namereq: &'c RefCell<HashSet<Rc<str>>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            seq: Cell::new(0),
            namereg: RefCell::new(HashSet::new()),
        }
    }

    /// Creates an empty child Ctx
    pub fn create_empty_ctx(&self) -> Ctx {
        Ctx {
            args: &[],
            ret: None,
            seq: &self.seq,
            namereq: &self.namereg
        }
    }
}

impl<'a, 'r, 'c> Ctx<'a, 'r, 'c> {

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

    pub fn get_next(&self) -> usize {
        let current = self.seq.get();
        self.seq.set(current + 1);
        current
    }

    /// Register a new string
    pub fn register_str(&self, name: &str) -> Rc<str> {
        let mut register = self.namereq.borrow_mut();
        if !register.contains(name) {
            register.insert(Rc::from(name));
        }
        Rc::clone(register.get(name).unwrap())
    }
}
