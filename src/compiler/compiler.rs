use super::{
    parser,
    instructions::{
        Ins,
        Arg,
        Type,
        Operation,
        Comparison
    },
    ast::{
        Statement,
        Argument,
    },
    error::MlogsError,
    compiler_utils::*,
};

pub fn compile(source: &str) -> Result<String, String> {
    let tokens: Result<Statement, MlogsError> = parser::tokenize(source);
    match tokens {
        Ok(tkns) => {
            match compile_statement(&tkns, None) {
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
                Label(_) => {
                    if inst == ins.len() - 1 {
                        "end".into()
                    } else {
                        continue
                    }
                },
                GetLink([res, arg]) => format!("getlink {} {}", res, arg),
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
                Set([name, value]) => format!("set {} {}", name, value),
                End => "end".into(),
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

            // Combines op and set
            if let Some(new_instruction) = combine_op_and_set(&ins[it], &ins[it + 1]) {
                res.push(new_instruction);
                it += 2;
                continue;
            }
        }

        res.push(cin.clone());
        it += 1;
    }

    res
}

pub fn compile_statement(statement: &Statement, ret: Option<&Vec<&str>>) -> Result<Vec<Ins>, String> {
    use std::str::FromStr;
    let mut ins = Vec::new();
    let mut matched = true;
    match *statement.command {
        name @ "print" => {
            let (mut i, [a]) = generic_passthrough::<1>(name, &statement.arguments)?;
            i.push(Ins::Print(a));
            ins.extend(i);
        },
        "control" => ins.extend(control_function(&statement.arguments)?),
        "do" => ins.extend(do_function(&statement.arguments)?),
        "getlink" => ins.extend(getlink_function(&statement.arguments, ret)?),
        "iterlinks" => ins.extend(iterlinks_function(&statement.arguments)?),
        "printflush" => ins.push(printflush_function(&statement.arguments)?),
        "while" => ins.extend(while_function(&statement.arguments)?),
        "set" => ins.extend(set_function(&statement.arguments)?),
        "sensor" => ins.extend(sensor_function(&statement.arguments, ret)?),
        "_raw" => ins.push(raw_function(&statement.arguments)?),
        _ => matched = false
    };

    if matched {
        Ok(ins)
    } else {
        if let Ok(op) = Operation::from_str(*statement.command) {
            ins.extend(make_expression(op, &statement.arguments, ret)?);
            return Ok(ins);
        }

        return Err(format!("Unknown command: {}", statement.command.fragment()))
    }
}

fn set_function(args: &[Argument]) -> Result<Vec<Ins>, String> {
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
            let mut ins = compile_statement(stmnt, Some(&vec![&ret]))?;
            ins.push(Set([make_variable(ident), str_to_var(ret)]));

            return Ok(ins);
        }
    }])])
}

fn do_function(args: &[Argument]) -> Result<Vec<Ins>, String> {
    let mut ins = Vec::new();
    for arg in args {
        if let Argument::Statement(stmnt) = arg {
            match compile_statement(stmnt, None) {
                Ok(new_ins) => ins.extend(new_ins),
                Err(err) => return Err(err)
            }
        } else {
            return Err("do function only accepts statements as arguments".into())
        }
    }
    Ok(ins)
}

fn raw_function(args: &[Argument]) -> Result<Ins, String> {
    if args.len() != 1 {
        return Err("_raw expects exactly one argument".into());
    }

    if let Argument::String(span) = args[0] {
        let r = *span;
        Ok(Ins::_Raw((r[1..r.len() - 1]).into()))
    } else {
        Err("_raw argument must be a string!".into())
    }
}

pub fn make_expression(opr: Operation, args: &[Argument], ret: Option<&Vec<&str>>) -> Result<Vec<Ins>, String> {
    if opr.unary() && args.len() != 1 {
        return Err(format!("Expression {:?} accepts exactly two arguments", opr));
    } else if !opr.unary() && args.len() != 2 {
        return Err(format!("Expression {:?} accepts exactly one argument", opr));
    } else if !(1..=2).contains(&args.len()) {
        return Err("Expressions can only have one or two arguments".into());
    }

    // A slight optimization
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

    if let Some(returnarg) = get_return_for_expression(ret) {
        ins.push(Ins::Op(opr,[returnarg, arg0, if opr.unary() { Arg::Literal(Type::Num(0.0)) } else { arg1.unwrap() }]))
    }

    Ok(ins)
}

fn make_expression_argument(arg: &Argument) -> Result<(Arg, Option<Vec<Ins>>), String> {
    use Argument::*;
    Ok((match arg {
        Identifier(ident) => make_variable(ident),
        Number(num) => make_literal_num(num),
        String(string) => make_literal_str(string),
        Statement(stmnt) => {
            let ret0 = generate_variable();
            let ins = compile_statement(stmnt, Some(&vec![&ret0]))?;
            return Ok((str_to_var(ret0), Some(ins)));
        }
    }, None))
}

fn while_function(args: &[Argument]) -> Result<Vec<Ins>, String> {
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
            condition_ins = Some(compile_statement(stmnt, Some(&vec![&ret]))?);
            str_to_var(ret)
        }
    };

    let loop_ins = if let Statement(stmnt) = &args[1] {
        compile_statement(stmnt, None)?
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
    ins.push(Ins::Jump(forward_label, Comparison::Equals, [condition, Arg::Literal(Type::Num(0.0))]));

    ins.extend(loop_ins);

    ins.push(Ins::Jump(back_label, Comparison::Always, [zero(), zero()]));
    ins.push(Ins::Label(forward_label));

    Ok(ins)
}

/// Cleanly passes through N arguments
pub fn generic_passthrough<const N: usize>(funcname: &str, args: &[Argument]) -> Result<(Vec<Ins>, [Arg; N]), String> {
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
                let ret = generate_variable();
                ins.extend(compile_statement(stmnt, Some(&vec![&ret]))?);
                str_to_var(ret)
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

fn getlink_function(args: &[Argument], ret: Option<&Vec<&str>>) -> Result<Vec<Ins>, String> {
    if args.len() != 1 {
        return Err("getlink function accepts exactly one argument".into())
    }

    let mut ins = Vec::new();

    let arg = match &args[0] {
        Argument::Identifier(ident) => make_variable(ident),
        Argument::Number(num) => make_literal_num(num),
        Argument::String(_) => return Err("The argument to getlink function cannot be a string".into()),
        Argument::Statement(stmnt) => {
            let ret = generate_variable();
            ins.extend(compile_statement(stmnt, Some(&vec![&ret]))?);
            str_to_var(ret)
        }
    };

    ins.push(Ins::GetLink([ret_or_null(ret), arg]));

    Ok(ins)
}

fn iterlinks_function(args: &[Argument]) -> Result<Vec<Ins>, String> {
    if args.len() != 1 {
        return Err("iterlinks function accepts exactly one argument".into())
    }

    let mut ins = Vec::new();
    let statement_ins =
        if let Argument::Statement(stmnt) = &args[0] {
            let ret = generate_variable();
            compile_statement(stmnt, Some(&vec![&ret]))?
        } else {
            return Err("iterlinks argument must be a statement".into());
    };

    let itervar = generate_variable();
    let repeat_label = generate_label();
    let exit_label = generate_label();

    ins.push(Ins::Set([str_to_var(&itervar), zero()]));
    ins.push(Ins::Label(repeat_label));
    ins.push(Ins::GetLink([str_to_var("link"), str_to_var(&itervar)]));
    ins.push(Ins::Jump(exit_label, Comparison::StrictEquals, [str_to_var(&itervar), str_to_var("null")]));
    ins.extend(statement_ins);
    ins.push(Ins::Op(Operation::Plus, [str_to_var(&itervar), str_to_var(&itervar), one()]));
    ins.push(Ins::Jump(repeat_label, Comparison::LessThan, [str_to_var(&itervar), str_to_var("@links")]));
    ins.push(Ins::Label(exit_label));

    Ok(ins)
}

fn printflush_function(args: &[Argument]) -> Result<Ins, String> {
    if args.len() != 1 {
        return Err("printflush function accepts exactly one argument".into())
    }

    if let Argument::Identifier(ident) = &args[0] {
        Ok(Ins::PrintFlush(make_variable(ident)))
    } else {
        Err("printflush argument must be a variable".into())
    }
}

fn sensor_function(args: &[Argument], ret: Option<&Vec<&str>>) -> Result<Vec<Ins>, String> {
    todo!("sensor")
}

fn control_function(args: &[Argument]) -> Result<Vec<Ins>, String> {
    todo!("control")
}
