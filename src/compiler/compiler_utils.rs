use super::{
    ast::*,
    common::Span,
    instructions::{
        self,
        Arg::{self, *},
        Ins,
        Type
    },
    compilation::compile_statement,
    context::Ctx
};

// TODO Put this into Ctx, or make it somehow specific for every compilation
lazy_static::lazy_static! {
    static ref VARSEQ: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    static ref LABELSEQ: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
}

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
pub fn make_literal_str<T: ToString>(span: T) -> Arg {
    Literal(Type::Str(span.to_string()))
}

/// Creates a newline literal string
pub fn newline() -> Arg {
    make_literal_str("\\n")
}

/// Converts a span into a number, without checking if it is a valid number
pub fn make_literal_num<'a, T: std::borrow::Borrow<Span<'a>>>(span: T) -> Arg {
    Literal(Type::Num(span.borrow().parse().unwrap()))
}

/// Converts a statement into instructions and a generated variable argument
pub fn make_statement(ctx: Ctx, stmnt: &Statement) -> Result<(Arg, Vec<Ins>), String> {
    let ret = generate_variable();
    let ins = compile_statement(stmnt, ctx.with_ret(&ret))?;
    Ok((str_to_var(ret), ins))
}

/// Converts an argument into anything but a string
pub fn make_not_string(ctx: Ctx, arg: &Argument, err: &str) -> Result<(Arg, Vec<Ins>), String> {
    let mut ins = Vec::new();
    let newarg = match arg {
        Argument::String(_) => return Err(err.into()),
        Argument::Number(num) => make_literal_num(num),
        Argument::Identifier(ident) => make_variable(ident),
        Argument::Statement(stmnt) => {
            let (arg, newins) = make_statement(ctx, stmnt)?;
            ins.extend(newins);
            arg
        }
    };

    Ok((newarg, ins))
}

/// Converts an argument into an Arg
pub fn make_generic(ctx: Ctx, arg: &Argument) -> Result<(Arg, Vec<Ins>), String> {
    let mut ins = Vec::new();
    let newarg = match arg {
        Argument::String(span) => make_literal_str(span),
        Argument::Number(num) => make_literal_num(num),
        Argument::Identifier(ident) => make_variable(ident),
        Argument::Statement(stmnt) => {
            let (arg, newins) = make_statement(ctx, stmnt)?;
            ins.extend(newins);
            arg
        }
    };

    Ok((newarg, ins))
}

pub fn make_num<T : Into<f64>>(n: T) -> Arg {
    Arg::Literal(Type::Num(n.into()))
}

pub fn zero() -> Arg {
    make_num(0)
}

pub fn one() -> Arg {
    make_num(1)
}

pub fn null() -> Arg {
    str_to_var("null")
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

/// Always jumps to the label
pub fn jump_to(label: usize) -> Ins {
    Ins::Jump {
        label,
        cmp: instructions::Comparison::Always,
        left: zero(),
        right: zero(),
    }
}

pub fn generate_label() -> usize {
    use std::sync::atomic::Ordering;
    LABELSEQ.fetch_add(1, Ordering::Relaxed)
}

/// Returns a variable argument to return to, or null
pub fn ret_or_null(ret: Option<&str>) -> Arg {
    match ret {
        Some(s) => str_to_var(s),
        None => null()
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
