use super::{
    ast::*,
    common::Span,
    instructions::{
        Arg::{self, *},
        Type
    }
};

lazy_static::lazy_static! {
    static ref VARSEQ: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    static ref LABELSEQ: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
}

use log::warn;

pub fn expect_identifier<'a, 'b>(arg: &'a Argument, err: &'b str) -> Result<Span<'a>, String> {
    match arg {
        Argument::Identifier(span) => Ok(*span),
        _ => Err(err.into())
    }
}

/// Converts a span into a Variable
pub fn make_variable<'a, T: std::borrow::Borrow<Span<'a>>>(span: T) -> Arg {
    Variable((**span.borrow()).into())
}

/// Converts a span into a Literal
pub fn make_literal_str<'a, T: std::borrow::Borrow<Span<'a>>>(span: T) -> Arg {
    Literal(Type::Str((**span.borrow()).into()))
}

/// Converts a span into a number, without checking if it is a valid number
pub fn make_literal_num<'a, T: std::borrow::Borrow<Span<'a>>>(span: T) -> Arg {
    Literal(Type::Num(span.borrow().parse().unwrap()))
}

pub fn make_num(n: f64) -> Arg {
    Arg::Literal(Type::Num(n))
}

pub fn zero() -> Arg {
    Arg::Literal(Type::Num(0.0))
}

pub fn one() -> Arg {
    Arg::Literal(Type::Num(1.0))
}

pub fn str_to_var<T: std::convert::AsRef<str>>(string: T) -> Arg {
    Variable(string.as_ref().to_owned())
}

/// Generates a random variable name for use in
/// the resulting mlog code
pub fn generate_variable() -> String {
    if crate::ARGS.seqvars {
        use std::sync::atomic::Ordering;
        let var = format!("__{}", VARSEQ.fetch_add(1, Ordering::Relaxed));
        var
    } else {
        use rand::Rng;
        let mut var = String::with_capacity(crate::ARGS.varlen + 2);
        var.push_str("__");

        const CHARSET: &[u8] = b"0123456789abcdef";
        let mut rng_lock = crate::RNG.lock().unwrap();
        let rng = rng_lock.as_mut().unwrap();
        for _ in 0..crate::ARGS.varlen {
            var.push(CHARSET[rng.gen_range(0..CHARSET.len())] as char)
        }

        var
    }
}

pub fn generate_label() -> usize {
    use std::sync::atomic::Ordering;
    LABELSEQ.fetch_add(1, Ordering::Relaxed)
}

/// Returns a variable to return to, or none
pub fn get_return_for_expression(ret: Option<&Vec<&str>>) -> Option<Arg> {
    if let Some(v) = ret {
        if v.is_empty() {
            return None;
        }
        if v.len() > 1 {
            warn!("Received more than one return variable for binary operation");
        }

        Some(Variable(v[0].into()))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn variables() {
        use crate::compiler::compiler_utils::generate_variable;
        let var = generate_variable();
        assert_eq!(var.len(), 34);
        assert_eq!(&var[0..=1], "__");
        hex::decode(&var[2..]).expect("Not hex");
    }
}
