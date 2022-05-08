use super::{
    parser,
    instructions::{
        self,
        Ins,
        Arg,
        Type,
        Operation,
        Comparison
    },
    ast::{ self, Argument },
    error::ParserError,
    compiler_utils::*,
    context::Ctx
};

use log::{ warn, debug };

pub fn compile(source: &str) -> Result<String, String> {
    let tokens: Result<ast::Statement, ParserError> = parser::tokenize(source);
    match tokens {
        Ok(tkns) => {
            match compile_statement(&tkns, Ctx::empty()) {
                Ok(ins) => {
                    debug!("Instructions: {:#?}", ins);
                    translate(&ins).map(|r| r.join("\n"))
                },
                Err(err) => Err(err)
            }
        },
        Err(err) => {
            Err(err.message)
        }
    }
}

/// Translates instructions to their mlog counterparts
pub fn translate(ins: &[Ins]) -> Result<Vec<String>, String> {
    use std::collections::HashMap;

    let optimized_ins = repeated_optimize(ins);
    let ins = optimized_ins.as_deref().unwrap_or(ins);

    // Translated instructions
    let mut result: Vec<String> = Vec::new();
    let labels: HashMap<usize, usize> = { // Filling in labels
        let mut lbls = HashMap::new();
        let mut depth = 0;
        for inst in ins {
            depth += inst.size();
            if let Ins::Label(label) = inst {
                let previous = lbls.insert(*label, depth);
                if previous.is_some() {
                    return Err("A label appeared for a second time, this should never happen! Contact author(s)".into())
                }
            }
        }
        lbls
    };

    for inst in 0..ins.len() {
        use Ins::*;
        result.push(
            match &ins[inst] {
                Control { target, subcommand } => {
                    use instructions::ControlSI::*;
                    match subcommand {
                        Enabled(enabled) => format!("control enabled {} {}", target, enabled),
                        Shoot { x, y, shoot } => format!("control shoot {} {} {} {}", target, x, y, shoot),
                        Shootp { unit, shoot } => format!("control shootp {} {} {}", target, unit, shoot),
                        Configure(configuration) => format!("control configure {} {}", target, configuration),
                        Color { r, g, b } => format!("control color {} {} {} {}", target, r, g, b)
                    }
                },
                End => "end".into(),
                Label(_) => {
                    if inst == ins.len() - 1 {
                        "end".into()
                    } else {
                        continue
                    }
                },
                GetLink { store, index } => format!("getlink {} {}", store, index),
                Jump { label, cmp, left, right } => {
                    if !labels.contains_key(label) {
                        return Err("Missing label, this should never happen! Contact author(s)".into())
                    }
                    let cmpstr: &'static str = (*cmp).into();
                    format!("jump {} {} {} {}", labels[label], cmpstr, left, right)
                },
                Print(arg) => format!("print {}", arg),
                PrintFlush(arg) => format!("printflush {}", arg),
                Op { op, result: ret, left, right } => format!("op {} {} {} {}", op.to_string(), ret, left, right),
                Radar{ from, order, result: ret, sort, conds } => {
                    format!("radar {} {} {} {} {} {} {}",
                        conds[0], conds[1], conds[2],
                        sort, from, order, ret)
                },
                Sensor([store, block, sensable]) => format!("sensor {} {} {}", store, block, sensable),
                Set { variable, value } => format!("set {} {}", variable, value),
                UnitBind(unit) => format!("ubind {}", unit),
                _Raw(s) => s.clone(),
                instruction => return Err(format!("Instruction {:?} not yet implemented for translation", instruction))
            }
        )
    }

    Ok(result)
}

pub fn optimize(ins: &[Ins]) -> Vec<Ins> {
    let mut res: Vec<Ins> = Vec::with_capacity(ins.len());

    let mut it = 0;
    while it < ins.len() {
        // Current INstruction
        let cin: &Ins = &ins[it];

        // We need two instructions
        if it != ins.len() - 1 {
            // Combines operations and jumps if possible
            if let Some(new_instruction) = try_combine_op_and_jump(&ins[it], &ins[it + 1]) {
                res.push(new_instruction);
                it += 2;
                continue;
            }

            // Combines radar and set if possible
            if let Some(new_instruction) = combine_radar_and_set(&ins[it], &ins[it + 1]) {
                res.push(new_instruction);
                it += 2;
                continue;
            }

            // Combines op and set
            if let Some(new_instruction) = combine_op_and_set(&ins[it], &ins[it + 1]) {
                res.push(new_instruction);
                it += 2;
                continue;
            }

            /* TODO Optimize getlink and set into one,
             * this will require checking for temporary variables and variable count,
             * so for now, it is just a todo.
             */
        }

        res.push(cin.clone());
        it += 1;
    }

    res
}

/// Repeats [`optimize`] until it changes nothing
pub fn repeated_optimize(ins: &[Ins]) -> Option<Vec<Ins>> {
    if crate::ARGS.transopts {
        let mut old_oins = optimize(ins);
        loop {
            let oins = optimize(&old_oins);
            if oins == old_oins {
                break Some(oins);
            } else {
                old_oins = oins;
            }
        }
    } else {
        None
    }
}

/// The standard function to compile a [`ast::Statement`]
///
/// Replaces the `ctx`'s arguments 
pub fn compile_statement(statement: &ast::Statement, ctx: Ctx) -> Result<Vec<Ins>, String> {
    use std::str::FromStr;
    let mut ins = Vec::new();
    let mut matched = true;
    let newctx = ctx.with_args(&statement.arguments);
    match *statement.command {
        "bind" => ins.push(bind_function(newctx)?),
        "control" => ins.extend(control_function(newctx)?),
        "do" => ins.extend(do_function(newctx)?),
        "getlink" => ins.extend(getlink_function(newctx)?),
        "if" => ins.extend(if_function(newctx)?),
        "iterlinks" => ins.extend(iterlinks_function(newctx)?),
        "printflush" => ins.push(printflush_function(newctx)?),
        "radar" => ins.extend(radar_function(newctx)?),
        "set" => ins.extend(set_function(newctx)?),
        "sensor" => ins.push(sensor_function(newctx)?),
        "print" => {
            let (mut i, [a]) = generic_passthrough::<1>("print", newctx)?;
            i.push(Ins::Print(a));
            ins.extend(i);
        },
        "println" => {
            let (mut i, [a]) = generic_passthrough::<1>("println", newctx)?;
            i.push(Ins::Print(a));
            i.push(Ins::Print(newline()));
            ins.extend(i);
        },
        "while" => ins.extend(while_function(newctx)?),
        "_raw" => ins.push(raw_function(newctx)?),
        _ => matched = false
    };

    if matched {
        Ok(ins)
    } else {
        if let Ok(op) = Operation::from_str(*statement.command) {
            ins.extend(make_expression(op, newctx)?);
            return Ok(ins);
        }

        return Err(format!("Unknown command: {}", statement.command.fragment()))
    }
}

fn set_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    let args = ctx.args;
    use Argument::*;

    if args.len() != 2 {
        return Err("set function accepts exactly two arguments".into());
    }

    let ident = expect_identifier(&args[0], "first argument to set must be an identifier")?;

    let (value, mut ins) = make_generic(ctx, &args[1])?;
    ins.push(Ins::Set { variable: make_variable(ident), value });

    Ok(ins)
}

fn do_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    let args = ctx.args;
    let mut ins = Vec::new();
    for arg in args {
        if let Argument::Statement(stmnt) = arg {
            match compile_statement(stmnt, ctx.no_ret()) {
                Ok(new_ins) => ins.extend(new_ins),
                Err(err) => return Err(err)
            }
        } else {
            return Err("do function only accepts statements as arguments".into())
        }
    }
    Ok(ins)
}

fn raw_function(ctx: Ctx) -> Result<Ins, String> {
    let args = ctx.args;
    if args.len() != 1 {
        return Err("_raw expects exactly one argument".into());
    }

    if let Argument::String(span) = args[0] {
        let r = *span;
        Ok(Ins::_Raw((r[1..r.len() - 1]).into()))
    } else {
        Err("_raw argument must be a string".into())
    }
}

pub fn make_expression(op: Operation, ctx: Ctx) -> Result<Vec<Ins>, String> {
    let Ctx { args, ret, .. } = ctx;
    if op.unary() && args.len() != 1 {
        return Err(format!("Expression {:?} accepts exactly two arguments", op));
    } else if !op.unary() && args.len() != 2 {
        return Err(format!("Expression {:?} accepts exactly one argument", op));
    } else if !(1..=2).contains(&args.len()) {
        return Err("Expressions can only have one or two arguments".into());
    }

    // A slight optimization
    // NOTE This should probably be in `optimize`, but for now is here
    if ret.is_none() && !args[0].is_statement() && !args[1].is_statement() {
        return Ok(vec![]);
    }

    let (left, mut ins) = make_generic(ctx, &args[0])?;

    let right = if !op.unary() {
        let (arg1, ins1) = make_generic(ctx, &args[1])?;
        ins.extend(ins1);
        arg1
    } else {
        zero()
    };

    ins.push(Ins::Op { op, result: ret_or_null(ret), left, right });

    Ok(ins)
}

fn while_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    let args = ctx.args;
    if args.len() != 2 {
        return Err("while function accepts exactly two arguments".into());
    }

    let (condition, condition_ins) = make_generic(ctx, &args[0])?;

    let loop_ins = if let Argument::Statement(stmnt) = &args[1] {
        compile_statement(stmnt, ctx.no_ret())?
    } else {
        return Err("Second argument to while must be a statement".into());
    };

    let mut ins = Vec::new();


    let back_label = generate_label();
    let forward_label = generate_label();
    ins.push(Ins::Label(back_label));
    ins.extend(condition_ins);
    ins.push(Ins::Jump { label: forward_label, cmp: Comparison::StrictEquals, left: condition, right: zero() });

    ins.extend(loop_ins);

    ins.push(Ins::Jump { label: back_label, cmp: Comparison::Always, left: zero(), right: zero() });
    ins.push(Ins::Label(forward_label));

    Ok(ins)
}

/// Cleanly passes through N arguments
pub fn generic_passthrough<const N: usize>(funcname: &str, ctx: Ctx) -> Result<(Vec<Ins>, [Arg; N]), String> {
    let args = ctx.args;

    let mut ins = Vec::new();
    let mut argvec = Vec::with_capacity(N);

    if args.len() != N {
        return Err(format!("{} accepts exactly {} arguments", funcname, N));
    }

    for arg in args {
        let (newarg, newins) = make_generic(ctx, arg)?;
        argvec.push(newarg);
        ins.extend(newins);
    }

    Ok((ins, argvec.try_into().unwrap()))
}

fn try_combine_op_and_jump(ins1: &Ins, ins2: &Ins) -> Option<Ins> {
    match (ins1, ins2) {
        ( Ins::Op { op: opr, result, left: oarg0, right: oarg1 },
          Ins::Jump { label, cmp, left: jarg0, right: jarg1 } ) => {
            if !matches!(cmp, Comparison::StrictEquals) {
                return None
            }

            let opr_as_cmp: Comparison = (*opr).try_into().ok()?;
            if opr_as_cmp == Comparison::StrictEquals || opr_as_cmp == Comparison::Always {
                return None;
            }

            if result == jarg0 {
                if let Arg::Literal(Type::Num(other)) = jarg1 {
                    if *other == 0.0 {
                        return Some(
                            Ins::Jump {
                                label: *label,
                                cmp: -opr_as_cmp,
                                left: oarg0.clone(),
                                right: oarg1.clone()
                            });
                    }
                }
            }

            if result == jarg1 {
                if let Arg::Literal(Type::Num(other)) = jarg0 {
                    if *other == 0.0 {
                        return Some(
                            Ins::Jump {
                                label: *label,
                                cmp: -opr_as_cmp,
                                left: oarg0.clone(),
                                right: oarg1.clone()
                            });
                    }
                }
            }
        },
        _ => return None
    };
    None
}

fn combine_op_and_set(ins1: &Ins, ins2: &Ins) -> Option<Ins> {
    match (ins1, ins2) {
        ( Ins::Op { op, result: Arg::Variable(resvar), left, right },
          Ins::Set { variable, value: Arg::Variable(setvalue) } ) => {
            if resvar == setvalue {
                return Some(Ins::Op {
                    op: *op, result: variable.clone(), left: left.clone(), right: right.clone()
                });
            }
        },
        _ => return None
    };
    None
}

fn getlink_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    let Ctx { args, ret, .. } = ctx;
    if args.len() != 1 {
        return Err("getlink function accepts exactly one argument".into())
    }

    let (arg, mut ins) = make_not_string(ctx, &args[0], "The argument to getlink function cannot be a string")?;
    ins.push(Ins::GetLink{store: ret_or_null(ret), index: arg});
    Ok(ins)
}

fn iterlinks_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    let args = ctx.args;
    if args.len() != 1 {
        return Err("iterlinks function accepts exactly one argument".into())
    }

    let mut ins = Vec::new();
    let statement_ins =
        if let Argument::Statement(stmnt) = &args[0] {
            let ret = generate_variable();
            compile_statement(stmnt, ctx.with_ret(&ret))?
        } else {
            return Err("iterlinks argument must be a statement".into());
    };

    let itervar = generate_variable();
    let repeat_label = generate_label();
    let exit_label = generate_label();

    ins.push(Ins::Set { variable: str_to_var(&itervar), value: zero() });
    ins.push(Ins::Label(repeat_label));
    ins.push(Ins::GetLink { store: str_to_var("link"), index: str_to_var(&itervar) });
    ins.push(Ins::Jump {
        label: exit_label,
        cmp: Comparison::StrictEquals,
        left: str_to_var(&itervar),
        right: str_to_var("null")
    });
    ins.extend(statement_ins);
    ins.push(Ins::Op {
        op: Operation::Plus,
        result: str_to_var(&itervar),
        left: str_to_var(&itervar),
        right: one()
    });
    ins.push(Ins::Jump {
        label: repeat_label,
        cmp: Comparison::LessThan,
        left: str_to_var(&itervar),
        right: str_to_var("@links")
    });
    ins.push(Ins::Label(exit_label));

    Ok(ins)
}

fn printflush_function(ctx: Ctx) -> Result<Ins, String> {
    let args = ctx.args;
    if args.len() != 1 {
        return Err("printflush function accepts exactly one argument".into())
    }

    if let Argument::Identifier(ident) = &args[0] {
        Ok(Ins::PrintFlush(make_variable(ident)))
    } else {
        Err("printflush argument must be a variable".into())
    }
}

fn sensor_function(ctx: Ctx) -> Result<Ins, String> {
    let Ctx { args, ret, .. } = ctx;
    if args.len() != 2 {
        return Err("sensor function accepts exactly two arguments".into());
    }

    let block = make_variable(expect_identifier(&args[0], "first argument `block' to sensor must be an identifier")?);
    let sensable = expect_identifier(&args[1], "second argument `sensable' to sensor must be an identifier")?;
    if sensable.as_bytes()[0] != b'@' {
        warn!("second argument `sensable' to sensor should probably begin with a @");
    }

    let sensable = make_variable(sensable);
    Ok(Ins::Sensor([ret_or_null(ret), block, sensable]))
}

fn control_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    use instructions::ControlSI;
    let args = ctx.args;
    if !matches!(args.len(), 2..=5) {
        return Err("control accepts between 2 and 5 arguments".into())
    }

    let mut ins = vec![];

    let target = make_variable(
        expect_identifier(&args[0], "first argument `target' to control function must be an identifier")?);
    let subcommand = expect_identifier(&args[1], "second argument `subcommand' to control function must be an identifier")?;

    let subcommand = match *subcommand {
        "enabled" => {
            const C: &str = "control subcommand `enabled'";

            if args.len() != 3 {
                return Err(format!("{} expects one argument", C));
            }

            let (enabled, instr) = make_not_string(ctx, &args[2], &format!("{} `enabled' argument cannot be a string", C))?;
            ins.extend(instr);
            ControlSI::Enabled(enabled)
        },
        "shoot" => {
            const C: &str = "control subcommand `shoot'";

            if args.len() != 5 {
                return Err(format!("{} expects three arguments", C));
            }

            let (x, instr) = make_not_string(ctx, &args[2], &format!("{} `x' argument cannot be a string", C))?;
            ins.extend(instr);
            let (y, instr) = make_not_string(ctx, &args[3], &format!("{} `y' argument cannot be a string", C))?;
            ins.extend(instr);
            let (shoot, instr) = make_not_string(ctx, &args[4], &format!("{} `shoot' argument cannot be a string", C))?;
            ins.extend(instr);

            ControlSI::Shoot { x, y, shoot }
        },
        "shootp" => {
            const C: &str = "control subcommand `shootp'";

            if args.len() != 4 {
                return Err(format!("{} expects two arguments", C));
            }

            let (unit, instr) = make_not_string(ctx, &args[2], &format!("{} `x' argument cannot be a string", C))?;
            ins.extend(instr);
            let (shoot, instr) = make_not_string(ctx, &args[3], &format!("{} `y' argument cannot be a string", C))?;
            ins.extend(instr);

            ControlSI::Shootp { unit, shoot }
        },
        "configure" => {
            const C: &str = "control subcommand `configure'";

            if args.len() != 3 {
                return Err(format!("{} expects one argument", C));
            }

            let (configuration, instr) =
                make_not_string(
                    ctx,
                    &args[2],
                    &format!("{} `configuration' argument cannot be a string", C)
                )?;
            ins.extend(instr);

            ControlSI::Configure(configuration)
        },
        "color" => {
            const C: &str = "control subcommand `shoot'";

            if args.len() != 5 {
                return Err(format!("{} expects three arguments", C));
            }

            let (r, instr) = make_not_string(ctx, &args[2], &format!("{} `r' argument cannot be a string", C))?;
            ins.extend(instr);
            let (g, instr) = make_not_string(ctx, &args[3], &format!("{} `g' argument cannot be a string", C))?;
            ins.extend(instr);
            let (b, instr) = make_not_string(ctx, &args[4], &format!("{} `b' argument cannot be a string", C))?;
            ins.extend(instr);

            ControlSI::Color{ r, g, b }
        },
        invalid => return Err(format!("Invalid control subcommand `{}'", invalid))
    };

    ins.push(Ins::Control{ target, subcommand});
    Ok(ins)
}

fn bind_function(ctx: Ctx) -> Result<Ins, String> {
    let args = ctx.args;
    if args.len() != 1 {
        return Err("bind expects exactly one argument".into());
    }

    let ident = expect_identifier(&args[0], "bind function's argument must be an identifier")?;
    if ident.as_bytes()[0] != b'@' {
        warn!("bind function's argument should probably begin with a @");
    }

    Ok(Ins::UnitBind(make_variable(ident)))
}

/// radar <from> <sort> <order> <prop> <prop> <prop>
fn radar_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    use instructions::{ TargetProp, TargetSort };
    let Ctx { args, ret, .. } = ctx;

    if args.len() > 6 {
        return Err("Radar accepts 6 or less arguments".into());
    }

    let mut ins = Vec::new();
    let mut from: Arg = str_to_var("@this");
    let mut order: Arg = make_num(1);
    let mut sort: TargetSort = TargetSort::Distance;
    let mut conds: [TargetProp; 3] = [TargetProp::Any, TargetProp::Any, TargetProp::Any];

    if !args.is_empty() {
        // First argument
        from = make_variable(expect_identifier(&args[0], "First argument to radar must be a string")?);

        if args.len() >= 2 {
            let ident = expect_identifier(&args[1], "The `sort` argument to radar must be an identifier")?;
            sort = ident.parse().map_err(|_| "Second argument to radar must be a valid sort")?;
        }

        if args.len() >= 3 {
            let (neworder, newins) = make_not_string(ctx, &args[2], "Third argument to radar cannot be a string")?;
            order = neworder;
            ins.extend(newins);
        }

        if args.len() >= 4 {
            let ident = expect_identifier(&args[3], "The first condition argument to radar must be an identifier")?;
            conds[0] = ident.parse().map_err(|_| "Fourth argument to radar must be a valid condition")?;
        }

        if args.len() >= 5 {
            let ident = expect_identifier(&args[4], "The second condition argument to radar must be an identifier")?;
            conds[1] = ident.parse().map_err(|_| "Fifth argument to radar must be a valid condition")?;
        }

        if args.len() >= 6 {
            let ident = expect_identifier(&args[5], "The second condition argument to radar must be an identifier")?;
            conds[2] = ident.parse().map_err(|_| "Sixth argument to radar must be a valid condition")?;
        }
    }

    ins.push(Ins::Radar {
        from,
        order,
        result: ret_or_null(ret),
        sort,
        conds
    });

    Ok(ins)
}

fn combine_radar_and_set(ins1: &Ins, ins2: &Ins) -> Option<Ins> {
    match (ins1, ins2) {
        ( Ins::Radar { result: Arg::Variable(resvar), .. },
          Ins::Set { variable, value: Arg::Variable(setvalue) }) => {
            if resvar == setvalue {
                let mut new_ins: Ins = ins1.clone();
                match new_ins {
                    Ins::Radar { ref mut result, .. } => *result = variable.clone(),
                    _ => unreachable!()
                };
                return Some(new_ins);
            }
        },
        _ => return None
    };
    None
}

fn if_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    let Ctx { args, ret, .. } = ctx;

    let mut argi = args.iter().peekable();

    let mut next_label = generate_label();
    let end_label = generate_label();

    // if part
    let (condition, mut ins) = make_generic(ctx, argi.next().ok_or("if function missing a condition")?)?;
    let (bodyresult, bodyins) = make_generic(ctx, argi.next().ok_or("if function missing a body")?)?;

    ins.push(Ins::Jump {
        label: next_label,
        cmp: Comparison::StrictEquals,
        left: condition,
        right: zero()
    });

    ins.extend(bodyins);
    ins.push(Ins::Set { variable: ret_or_null(ret), value: bodyresult });

    /* NOTE
     * While this should probably be an optimization,
     * it would be very difficult to detect at it's current
     * capabilities, so it is here.
     */
    if argi.len() != 0 {
        ins.push(jump_to(end_label));
    }

    let mut else_part = false;

    loop {
        match argi.peek() {
            Some(Argument::Identifier(ident)) => {
                argi.next(); // Skip what we just looked at
                match **ident {
                    "elif" => {
                        let (condition, condition_ins) = make_generic(ctx, argi.next().ok_or("elif missing a condition")?)?;
                        let (bodyresult, bodyins) = make_generic(ctx, argi.next().ok_or("elif missing a body")?)?;

                        ins.push(Ins::Label (next_label));
                        ins.extend(condition_ins);

                        next_label = generate_label();

                        ins.push(Ins::Jump {
                            label: next_label,
                            cmp: Comparison::StrictEquals,
                            left: condition,
                            right: zero()
                        });

                        ins.extend(bodyins);
                        ins.push(Ins::Set { variable: ret_or_null(ret), value: bodyresult });
                        ins.push(jump_to(end_label));
                    }
                    "else" => {
                        else_part = true;
                        break;
                    },
                    any => return Err(format!("unexpected identifier `{}` as argument to `if' function", any)),
                };
            },
            Some(_) => {
                let index = args.len() - argi.count();
                return Err(format!("argument {} of function `if' should be an identifier `elif` or `else`", index));
            },
            _ => break
        };
    }

    if else_part {
        let (bodyresult, bodyins) = make_generic(ctx, argi.next().ok_or("else missing a body")?)?;
        ins.push(Ins::Label (next_label));
        ins.extend(bodyins);
        ins.push(Ins::Set { variable: ret_or_null(ret), value: bodyresult });
    } else {
        ins.push(Ins::Label(next_label));
    }

    ins.push(Ins::Label(end_label));

    Ok(ins)
}
