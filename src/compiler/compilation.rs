use super::{
    parser,
    instructions::{
        self,
        Ins,
        Arg,
        Type,
        Operation,
        Comparison,
        ControlSI
    },
    ast::{
        Statement,
        Argument,
    },
    error::ParserError,
    compiler_utils::*,
    context::Ctx
};

use log::warn;

pub fn compile(source: &str) -> Result<String, String> {
    let tokens: Result<Statement, ParserError> = parser::tokenize(source);
    match tokens {
        Ok(tkns) => {
            match compile_statement(&tkns, Ctx::empty()) {
                Ok(ref ins) => translate(ins).map(|r| r.join("\n")),
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

    let optimized_ints: Option<Vec<Ins>>;
    if crate::ARGS.transopts {
        let mut old_oins = optimize(ins);
        loop {
            let oins = optimize(&old_oins);
            if oins == old_oins {
                break
            } else {
                old_oins = oins
            }
        }
        optimized_ints = Some(old_oins);
    } else {
        optimized_ints = None;
    }

    let optinsref: Option<&[Ins]> = optimized_ints.as_deref();
    let ins = optinsref.unwrap_or(ins);

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
                Control(target, subc) => {
                    use ControlSI::*;
                    match subc {
                        Enabled(enabled) => format!("control enabled {} {}", target, enabled),
                        Shoot([x, y, shoot]) => format!("control shoot {} {} {} {}", target, x, y, shoot),
                        Shootp([unit, shoot]) => format!("control shootp {} {} {}", target, unit, shoot),
                        Configure(configuration) => format!("control configure {} {}", target, configuration),
                        Color([r, g, b]) => format!("control color {} {} {} {}", target, r, g, b)
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
                Jump(label, cmp, [arg0, arg1]) => {
                    if !labels.contains_key(label) {
                        return Err("Missing label, this should never happen! Contact author(s)".into())
                    }
                    let cmpstr: &'static str = (*cmp).into();
                    format!("jump {} {} {} {}", labels[label], cmpstr, arg0, arg1)
                },
                Print(arg) => format!("print {}", arg),
                PrintFlush(arg) => format!("printflush {}", arg),
                Op(operation, [ret, arg0, arg1]) => format!("op {} {} {} {}", operation.to_string(), ret, arg0, arg1),
                Radar{ from, order, result: ret, sort, conds } => {
                    format!("radar {} {} {} {} {} {} {}",
                        conds[0], conds[1], conds[2],
                        sort, from, order, ret)
                },
                Sensor([store, block, sensable]) => format!("sensor {} {} {}", store, block, sensable),
                Set([name, value]) => format!("set {} {}", name, value),
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

/// The standard function to compile a `Statement`
///
/// Replaces the `ctx`'s arguments 
pub fn compile_statement(statement: &Statement, ctx: Ctx) -> Result<Vec<Ins>, String> {
    use std::str::FromStr;
    let mut ins = Vec::new();
    let mut matched = true;
    let newctx = ctx.with_args(&statement.arguments);
    match *statement.command {
        "bind" => ins.push(bind_function(newctx)?),
        "control" => ins.extend(control_function(newctx)?),
        "do" => ins.extend(do_function(newctx)?),
        "getlink" => ins.extend(getlink_function(newctx)?),
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
    use Ins::Set;
    use Argument::*;

    if args.len() != 2 {
        return Err("set function accepts exactly two arguments".into());
    }

    let ident = expect_identifier(&args[0], "first argument to set must be an identifier")?;

    Ok(vec![Set([make_variable(ident), match &args[1] {
        Identifier(id) => make_variable(id),
        Number(num) => make_literal_num(num),
        String(string) => make_literal_str(string),
        Statement(stmnt) => {
            let ret = generate_variable();
            let mut ins = compile_statement(stmnt, Ctx::just_ret(&ret))?;
            ins.push(Set([make_variable(ident), str_to_var(&ret)]));

            return Ok(ins);
        }
    }])])
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

pub fn make_expression(opr: Operation, ctx: Ctx) -> Result<Vec<Ins>, String> {
    let Ctx { args, ret, .. } = ctx;
    if opr.unary() && args.len() != 1 {
        return Err(format!("Expression {:?} accepts exactly two arguments", opr));
    } else if !opr.unary() && args.len() != 2 {
        return Err(format!("Expression {:?} accepts exactly one argument", opr));
    } else if !(1..=2).contains(&args.len()) {
        return Err("Expressions can only have one or two arguments".into());
    }

    // A slight optimization
    // NOTE This should probably be in `optimize`, but for now is here
    if ret.is_none() && !args[0].is_statement() && !args[1].is_statement() {
        return Ok(vec![]);
    }

    let mut ins = Vec::new();

    let (arg0, ins0) = make_expression_argument(&args[0])?;
    if let Some(i) = ins0 {
        ins.extend(i);
    }

    let arg1 = if !opr.unary() {
        let (arg1, ins1) = make_expression_argument(&args[1])?;
        if let Some(i) = ins1 {
            ins.extend(i)
        }
        Some(arg1)
    } else {
        None
    };

    ins.push(Ins::Op(opr,[ret_or_null(ret), arg0, arg1.unwrap_or_else(zero) ]));

    Ok(ins)
}

fn make_expression_argument(arg: &Argument) -> Result<(Arg, Option<Vec<Ins>>), String> {
    use Argument::*;
    Ok((match arg {
        Identifier(ident) => make_variable(ident),
        Number(num) => make_literal_num(num),
        String(string) => make_literal_str(string),
        Statement(stmnt) => {
            let ret = generate_variable();
            let ins = compile_statement(stmnt, Ctx::from_ret(&ret))?;
            return Ok((str_to_var(&ret), Some(ins)));
        }
    }, None))
}

fn while_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    let args = ctx.args;
    if args.len() != 2 {
        return Err("while function accepts exactly two arguments".into());
    }

    let mut condition_ins: Option<Vec<Ins>> = None;

    use Argument::*;
    let condition = match &args[0] {
        Identifier(id) => make_variable(id),
        Number(num) => make_literal_num(num),
        String(string) => make_literal_str(string),
        Statement(stmnt) => {
            let ret = generate_variable();
            condition_ins = Some(compile_statement(stmnt, ctx.with_ret(&ret))?);
            str_to_var(ret)
        }
    };

    let loop_ins = if let Statement(stmnt) = &args[1] {
        compile_statement(stmnt, ctx.no_ret())?
    } else {
        return Err("Second argument to while must be a statement".into());
    };

    let mut ins = Vec::new();


    let back_label = generate_label();
    let forward_label = generate_label();
    ins.push(Ins::Label(back_label));
    if let Some(cond_ins) = condition_ins {
        ins.extend(cond_ins);
    }
    ins.push(Ins::Jump(forward_label, Comparison::Equals, [condition, zero()]));

    ins.extend(loop_ins);

    ins.push(Ins::Jump(back_label, Comparison::Always, [zero(), zero()]));
    ins.push(Ins::Label(forward_label));

    Ok(ins)
}

/// Cleanly passes through N arguments
pub fn generic_passthrough<const N: usize>(funcname: &str, ctx: Ctx) -> Result<(Vec<Ins>, [Arg; N]), String> {
    let args = ctx.args;
    use Argument::*;
    let mut ins = Vec::new();
    let mut argvec = Vec::with_capacity(N);

    if args.len() != N {
        return Err(format!("{} accepts exactly {} arguments", funcname, N));
    }

    for arg in args {
        argvec.push(match arg {
            Identifier(ident) => make_variable(ident),
            Number(num) => make_literal_num(num),
            String(string) => make_literal_str(string),
            Statement(stmnt) => {
                let (newarg, newins) = make_statement(stmnt)?;
                ins.extend(newins);
                newarg
            }
        });
    }

    Ok((ins, argvec.try_into().unwrap()))
}

fn try_combine_op_and_jump(ins1: &Ins, ins2: &Ins) -> Option<Ins> {
    match (ins1, ins2) {
        ( Ins::Op(opr, [result, oarg0, oarg1]),
          Ins::Jump(label, cmp, [jarg0, jarg1])) => {
            if *cmp != Comparison::Equals {
                return None
            }

            let opr_as_cmp: Comparison = (*opr).try_into().ok()?;
            if opr_as_cmp == Comparison::StrictEquals || opr_as_cmp == Comparison::Always {
                return None;
            }

            if result == jarg0 {
                if let Arg::Literal(Type::Num(other)) = jarg1 {
                    if *other == 0.0 {
                        return Some(Ins::Jump(*label, -opr_as_cmp, [oarg0.clone(), oarg1.clone()]))
                    }
                }
            }

            if result == jarg1 {
                if let Arg::Literal(Type::Num(other)) = jarg0 {
                    if *other == 0.0 {
                        return Some(Ins::Jump(*label, -opr_as_cmp, [oarg0.clone(), oarg1.clone()]))
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
        ( Ins::Op(opr, [result, oarg0, oarg1]),
          Ins::Set([variable, value])) => {
            if let Arg::Variable(resvar) = result {
                if let Arg::Variable(setvalue) = value {
                    if resvar == setvalue {
                        return Some(Ins::Op(*opr, [variable.clone(), oarg0.clone(), oarg1.clone()]));
                    }
                }
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

    let (arg, mut ins) = make_not_string(&args[0], "The argument to getlink function cannot be a string")?;
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

    ins.push(Ins::Set([str_to_var(&itervar), zero()]));
    ins.push(Ins::Label(repeat_label));
    ins.push(Ins::GetLink { store: str_to_var("link"), index: str_to_var(&itervar) });
    ins.push(Ins::Jump(exit_label, Comparison::StrictEquals, [str_to_var(&itervar), str_to_var("null")]));
    ins.extend(statement_ins);
    ins.push(Ins::Op(Operation::Plus, [str_to_var(&itervar), str_to_var(&itervar), one()]));
    ins.push(Ins::Jump(repeat_label, Comparison::LessThan, [str_to_var(&itervar), str_to_var("@links")]));
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
    let args = ctx.args;
    if !matches!(args.len(), 2..=5) {
        return Err("control accepts between 2 and 5 arguments".into())
    }

    let mut ins = vec![];

    let target = make_variable(
        expect_identifier(&args[0], "first argument `target' to control function must be an identifier")?);
    let subcommand = expect_identifier(&args[1], "second argument `subcommand' to control function must be an identifier")?;

    match *subcommand {
        "enabled" => {
            const C: &str = "control subcommand `enabled'";

            if args.len() != 3 {
                return Err(format!("{} expects one argument", C));
            }

            let (enabled, instr) = make_not_string(&args[2], &format!("{} `enabled' argument cannot be a string", C))?;
            ins.extend(instr);
            ins.push(Ins::Control(target, ControlSI::Enabled(enabled)));
        },
        "shoot" => {
            const C: &str = "control subcommand `shoot'";

            if args.len() != 5 {
                return Err(format!("{} expects three arguments", C));
            }

            let (x, instr) = make_not_string(&args[2], &format!("{} `x' argument cannot be a string", C))?;
            ins.extend(instr);
            let (y, instr) = make_not_string(&args[3], &format!("{} `y' argument cannot be a string", C))?;
            ins.extend(instr);
            let (shoot, instr) = make_not_string(&args[4], &format!("{} `shoot' argument cannot be a string", C))?;
            ins.extend(instr);

            ins.push(Ins::Control(target, ControlSI::Shoot([x, y, shoot])));
        },
        "shootp" => {
            const C: &str = "control subcommand `shootp'";

            if args.len() != 4 {
                return Err(format!("{} expects two arguments", C));
            }

            let (unit, instr) = make_not_string(&args[2], &format!("{} `x' argument cannot be a string", C))?;
            ins.extend(instr);
            let (shoot, instr) = make_not_string(&args[3], &format!("{} `y' argument cannot be a string", C))?;
            ins.extend(instr);

            ins.push(Ins::Control(target, ControlSI::Shootp([unit, shoot])));
        },
        "configure" => {
            const C: &str = "control subcommand `configure'";

            if args.len() != 3 {
                return Err(format!("{} expects one argument", C));
            }

            let (configuration, instr) = make_not_string(&args[2], &format!("{} `configuration' argument cannot be a string", C))?;
            ins.extend(instr);

            ins.push(Ins::Control(target, ControlSI::Configure(configuration)));
        },
        "color" => {
            const C: &str = "control subcommand `shoot'";

            if args.len() != 5 {
                return Err(format!("{} expects three arguments", C));
            }

            let (r, instr) = make_not_string(&args[2], &format!("{} `r' argument cannot be a string", C))?;
            ins.extend(instr);
            let (g, instr) = make_not_string(&args[3], &format!("{} `g' argument cannot be a string", C))?;
            ins.extend(instr);
            let (b, instr) = make_not_string(&args[4], &format!("{} `b' argument cannot be a string", C))?;
            ins.extend(instr);

            ins.push(Ins::Control(target, ControlSI::Color([r, g, b])));
        },
        invalid => return Err(format!("Invalid control subcommand `{}'", invalid))
    };

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
            let (neworder, newins) = make_not_string(&args[2], "Third argument to radar cannot be a string")?;
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
        ( Ins::Radar { result, .. },
          Ins::Set([variable, value])) => {
            if let Arg::Variable(resvar) = result {
                if let Arg::Variable(setvalue) = value {
                    if resvar == setvalue {
                        let mut new_ins: Ins = ins1.clone();
                        match new_ins {
                            Ins::Radar { ref mut result, .. } => std::mem::replace(result, variable.clone()),
                            _ => unreachable!()
                        };
                        return Some(new_ins);
                    }
                }
            }
        },
        _ => return None
    };
    None
}
