use super::{
    ast::*,
    common::Span,
    instructions::{
        self,
        Arg::{self, *},
        Vartype,
        Ins,
        Type
    },
    compilation::compile_statement,
    context::Ctx
};

pub fn expect_identifier<'a, 'b>(arg: &'a Argument, err: &'b str) -> Result<Span<'a>, String> {
    match arg {
        Argument::Identifier(span) => Ok(*span),
        _ => Err(err.into())
    }
}

/// Converts a span into a Variable
pub fn make_variable<'a, T: std::borrow::Borrow<Span<'a>>>(span: T) -> Arg {
    Variable(Vartype::Named((**span.borrow()).into()))
}

/// Converts a span into a Literal
pub fn make_literal_str(ctx: Ctx, span: &Span) -> Arg {
    Literal(Type::Str(ctx.register_str(span)))
}

/// Creates a newline literal string
pub fn newline(ctx: Ctx) -> Arg {
    str_to_var(ctx, "\\n")
}

/// Converts a span into a number, without checking if it is a valid number
pub fn make_literal_num<'a, T: std::borrow::Borrow<Span<'a>>>(span: T) -> Arg {
    Literal(Type::Num(span.borrow().parse().unwrap()))
}

/// Converts a statement into instructions and a generated variable argument
pub fn make_statement(ctx: Ctx, stmnt: &Statement) -> Result<(Arg, Vec<Ins>), String> {
    let ret = generate_unnamed(ctx);
    let ins = compile_statement(stmnt, ctx.with_ret(&ret))?;
    Ok((Variable(ret), ins))
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
        Argument::String(span) => make_literal_str(ctx, span),
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

pub fn null(ctx: Ctx) -> Arg {
    str_to_var(ctx, "null")
}

pub fn str_to_var<T: std::convert::AsRef<str>>(ctx: Ctx, string: T) -> Arg {
    Variable(Vartype::Named(ctx.register_str(string.as_ref())))
}

/// Generates an unnamed variable for use in
/// the resulting mlog code
pub fn generate_variable(ctx: Ctx) -> Arg {
    Variable(generate_unnamed(ctx))
}

/// Generates the next unnamed vartype
pub fn generate_unnamed(ctx: Ctx) -> Vartype {
    Vartype::Unnamed(ctx.get_next())
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

pub fn generate_label(ctx: Ctx) -> usize {
    ctx.get_next()
}

/// Returns a variable argument to return to, or null
pub fn ret_or_null(ctx: Ctx, ret: Option<&Vartype>) -> Arg {
    match ret {
        Some(s) => Variable(s.clone()),
        None => null(ctx)
    }
}
