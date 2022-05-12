use super::{
    parser,
    instructions::{
        self,
        Ins,
        Arg,
        Operation,
        Comparison,
    },
    ast::{ self, Argument },
    error::ParserError,
    compiler_utils::*,
    context::{
        Context,
        Ctx,
    },
    settings::Settings,
    utils,
    optimizations,
};

use log::{ warn, debug, info };

pub fn compile(source: &str, settings: &Settings) -> Result<String, String> {
    let tokens: Result<ast::Statement, ParserError> = parser::tokenize(source);
    match tokens {
        Ok(tkns) => {
            let owned_ctx = Context::new();
            match compile_statement(&tkns, owned_ctx.create_empty_ctx()) {
                Ok(ins) => {
                    debug!("Instructions: {:#?}", ins);

                    info!("Creating PRNG Seed");
                    let seed = utils::create_seed(source);
                    info!("PRNG Seed: {}", hex::encode(seed));

                    translate(&ins, settings, &seed).map(|r| r.join("\n"))
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
///
/// seed is used to create random hexadecimal names
pub fn translate(ins: &[Ins], settings: &Settings, seed: &[u8; 32]) -> Result<Vec<String>, String> {
    use std::collections::HashMap;
    use rand::SeedableRng;

    let optimized_instructions = if settings.get_transopts() {
        let optimized = repeated_optimize(ins);
        debug!("Optimized instructions: {:#?}", optimized);
        Some(optimized)
    } else {
        None
    };

    let ins = optimized_instructions.as_deref().unwrap_or(ins);

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

    // Rng used to create random hex vars
    let mut rng = rand::rngs::StdRng::from_seed(*seed);
    // Map used to keep track of number to name relationships
    let mut map = HashMap::new();

    // A macro to aid us in this messy world
    macro_rules! disp {
        ($single:ident) => {
             $single.display(&mut rng, &mut map, settings)
        };
        ([$single:ident]) => {
            let $single = $single.display(&mut rng, &mut map, settings);
        };
        ([$single:ident, $($list:ident), +]) => {
            disp!([$single]);
            disp!([$($list), +]);
        };
    }

    for inst in 0..ins.len() {
        use Ins::*;
        result.push(
            match &ins[inst] {
                Control { target, subcommand } => {
                    disp!([target]);
                    use instructions::ControlSI::*;
                    match subcommand {
                        Enabled(enabled) => {
                            disp!([enabled]);
                            format!("control enabled {target} {enabled}")
                        },
                        Shoot { x, y, shoot } => {
                            disp!([x, y, shoot]);
                            format!("control shoot {target} {x} {y} {shoot}")
                        },
                        Shootp { unit, shoot } => {
                            disp!([unit, shoot]);
                            format!("control shootp {target} {unit} {shoot}")
                        },
                        Configure(configuration) => {
                            disp!([configuration]);
                            format!("control configure {target} {configuration}")
                        },
                        Color { r, g, b } => {
                            disp!([r, g, b]);
                            format!("control color {target} {r} {g} {b}")
                        },
                    }
                },
                Draw(subcommand) => {
                    use instructions::DrawSI::*;
                    match subcommand {
                        Clear { r, g, b } => {
                            disp!([r, g, b]);
                            format!("draw clear {r} {g} {b}")
                        },
                        Color { r, g, b, a } => {
                            disp!([r, g, b, a]);
                            format!("draw color {r} {g} {b} {a}")
                        },
                        Stroke(stroke) => {
                            disp!([stroke]);
                            format!("draw stroke {stroke}")
                        },
                        Line { x1, y1, x2, y2 } => {
                            disp!([x1, y1, x2, y2]);
                            format!("draw line {x1} {y1} {x2} {y2}")
                        },
                        Rect { x, y, width, height } => {
                            disp!([x, y, width, height]);
                            format!("draw rect {x} {y} {width} {height}")
                        },
                        LineRect { x, y, width, height } => {
                            disp!([x, y, width, height]);
                            format!("draw lineRect {x} {y} {width} {height}")
                        },
                        Poly { x, y, sides, radius, rotation } => {
                            disp!([x, y, sides, radius, rotation]);
                            format!("draw poly {x} {y} {sides} {radius} {rotation}")
                        },
                        LinePoly { x, y, sides, radius, rotation } => {
                            disp!([x, y, sides, radius, rotation]);
                            format!("draw linePoly {x} {y} {sides} {radius} {rotation}")
                        },
                        Triangle { x1, y1, x2, y2, x3, y3 } => {
                            disp!([x1, y1, x2, y2, x3, y3]);
                            format!("draw triangle {x1} {y1} {x2} {y2} {x3} {y3}")
                        },
                        Image { x, y, image, size, rotation } => {
                            disp!([x, y, image, size, rotation]);
                            format!("draw image {x} {y} {image} {size} {rotation}")
                        }
                    }
                }
                End => "end".into(),
                Label(_) => {
                    // Adds an `end` instruction at the end of the output if there isn't
                    // any instruction to jump to
                    if inst == ins.len() - 1 {
                        "end".into()
                    } else {
                        continue
                    }
                },
                GetLink { store, index } => {
                    disp!([store, index]);
                    format!("getlink {store} {index}")
                },
                Jump { label, cmp, left, right } => {
                    if !labels.contains_key(label) {
                        return Err("Missing label, this should never happen! Contact author(s)".into())
                    }
                    let cmpstr: &'static str = (*cmp).into();
                    disp!([left, right]);
                    let linenum = labels[label];
                    format!("jump {linenum} {cmpstr} {left} {right}")
                },
                Print(arg) => format!("print {}", disp!(arg)),
                PrintFlush(building) => {
                    format!("printflush {}", disp!(building))
                },
                Op { op, result: ret, left, right } => {
                    disp!([ret, left, right]);
                    let str_op = op.to_string();
                    format!("op {str_op} {ret} {left} {right}")
                },
                Radar { from, order, result: ret, sort, conds } => {
                    disp!([from, order, ret]);
                    let [cond1, cond2, cond3] = conds;
                    format!("radar {cond1} {cond2} {cond3} {sort} {from} {order} {ret}")
                },
                Sensor { result: ret, target, sensable } => {
                    disp!([ret, target, sensable]);
                    format!("sensor {ret} {target} {sensable}")
                },
                Set { variable, value } => {
                    disp!([variable, value]);
                    format!("set {variable} {value}")
                },
                UnitBind(unit) => format!("ubind {}", disp!(unit)),
                UnitRadar { order, result: ret, sort, conds } => {
                    disp!([order, ret]);
                    let [cond1, cond2, cond3] = conds;
                    format!("uradar {cond1} {cond2} {cond3} {sort} 0 {order} {ret}")
                },
                _Raw(s) => s.clone(),
                instruction => return Err(format!("Instruction {:?} not yet implemented for translation", instruction)),
            }
        )
    }

    Ok(result)
}

pub fn optimize(ins: &[Ins]) -> Vec<Ins> {
    use optimizations::*;
    let mut res: Vec<Ins> = Vec::with_capacity(ins.len());

    let mut it = 0;
    while it < ins.len() {
        // Current INstruction
        let cin: &Ins = &ins[it];

        // Checks if result of set or op is null, if so, removes them
        if check_null_set_or_op(cin) {
            // Removes current instruction
            it += 1;
            continue;
        }

        // We need two instructions
        if it != ins.len() - 1 {

            // Combines two set operations if the result of first
            // is the value of the second, and they're temporary variables
            if let Some(new_instruction) = combine_set_and_set(&ins[it], &ins[it + 1]) {
                res.push(new_instruction);
                it += 2;
                continue;
            }

            // Combines operations and jumps if possible
            if let Some(new_instruction) = combine_op_and_jump(&ins[it], &ins[it + 1]) {
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
pub fn repeated_optimize(ins: &[Ins]) -> Vec<Ins> {
    let mut old_oins = optimize(ins);
    let mut count = 1;
    loop {
        let oins = optimize(&old_oins);
        if oins == old_oins {
            debug!("Optimization cycles count: {}", count);
            break oins;
        } else {
            count += 1;
            old_oins = oins;
        }
    }
}

/// The standard function to compile a [`ast::Statement`]
///
/// Replaces the `ctx`'s arguments 
pub(crate) fn compile_statement(statement: &ast::Statement, ctx: Ctx) -> Result<Vec<Ins>, String> {
    use std::str::FromStr;
    let mut ins = Vec::new();
    let mut matched = true;
    let newctx = ctx.with_args(&statement.arguments);
    match *statement.command {
        "bind" => ins.push(bind_function(newctx)?),
        "control" => ins.extend(control_function(newctx)?),
        "do" => ins.extend(do_function(newctx)?),
        "draw" => ins.extend(draw_function(newctx)?),
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
        "uradar" => ins.extend(uradar_function(newctx)?),
        "while" => ins.extend(while_function(newctx)?),
        "_raw" => ins.push(raw_function(newctx)?),
        _ => matched = false,
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
    let Ctx { args, ret, .. } = ctx;

    if args.len() != 2 {
        return Err("set function accepts exactly two arguments".into());
    }

    let ident = expect_identifier(&args[0], "first argument to set must be an identifier")?;

    let (value, mut ins) = make_generic(ctx, &args[1])?;
    let var = make_variable(ident);
    ins.push(Ins::Set { variable: var.clone(), value });
    ins.push(Ins::Set { variable: ret_or_null(ret), value: var });

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

pub(crate) fn make_expression(op: Operation, ctx: Ctx) -> Result<Vec<Ins>, String> {
    let Ctx { args, ret, .. } = ctx;
    if op.unary() && args.len() != 1 {
        return Err(format!("Expression {:?} accepts exactly two arguments", op));
    } else if !op.unary() && args.len() != 2 {
        return Err(format!("Expression {:?} accepts exactly one argument", op));
    } else if !(1..=2).contains(&args.len()) {
        return Err("Expressions can only have one or two arguments".into());
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


    let back_label = generate_label(ctx);
    let forward_label = generate_label(ctx);
    ins.push(Ins::Label(back_label));
    ins.extend(condition_ins);
    ins.push(Ins::Jump { label: forward_label, cmp: Comparison::StrictEquals, left: condition, right: zero() });

    ins.extend(loop_ins);

    ins.push(Ins::Jump { label: back_label, cmp: Comparison::Always, left: zero(), right: zero() });
    ins.push(Ins::Label(forward_label));

    Ok(ins)
}

/// Cleanly passes through N arguments
pub(crate) fn generic_passthrough<const N: usize>(funcname: &str, ctx: Ctx) -> Result<(Vec<Ins>, [Arg; N]), String> {
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
            compile_statement(stmnt, ctx.no_ret())?
        } else {
            return Err("iterlinks argument must be a statement".into());
    };

    let itervar = generate_variable(ctx);
    let repeat_label = generate_label(ctx);
    let exit_label = generate_label(ctx);

    ins.push(Ins::Set { variable: itervar.clone(), value: zero() });
    ins.push(Ins::Label(repeat_label));
    ins.push(Ins::GetLink { store: str_to_var("link"), index: itervar.clone() });
    ins.push(Ins::Jump {
        label: exit_label,
        cmp: Comparison::StrictEquals,
        left: itervar.clone(),
        right: str_to_var("null")
    });
    ins.extend(statement_ins);
    ins.push(Ins::Op {
        op: Operation::Plus,
        result: itervar.clone(),
        left: itervar.clone(),
        right: one()
    });
    ins.push(Ins::Jump {
        label: repeat_label,
        cmp: Comparison::LessThan,
        left: itervar,
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

    let target = make_variable(expect_identifier(&args[0], "first argument `block' to sensor must be an identifier")?);
    let sensable = expect_identifier(&args[1], "second argument `sensable' to sensor must be an identifier")?;
    if !sensable.starts_with('@') {
        warn!("second argument `sensable' to sensor should probably begin with a @");
    }

    let sensable = make_variable(sensable);
    Ok(Ins::Sensor { result: ret_or_null(ret), target, sensable })
}

fn control_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    use instructions::ControlSI;
    let Ctx { args, .. } = ctx;
    if !matches!(args.len(), 2..=5) {
        return Err("control accepts between 2 and 5 arguments".into())
    }

    let mut ins = Vec::with_capacity(3);
    let mut argi = args.iter();

    let target = argi.next().ok_or("control missing required argument `target'")?;
    let target = make_variable(
        expect_identifier(target, "first argument `target' to control function must be an identifier")?);
    let subcmd = argi.next().ok_or("control missing required argument `subcommand'")?;
    let subcmd = expect_identifier(subcmd, "second argument `subcommand' to control function must be an identifier")?;

    // Macro to make this function slightly shorter
    macro_rules! notstr {
        ($subcommand:ident, $var:ident) => {
            let $var = {
                let var = argi.next().ok_or(
                    // draw subcommand missing `var'
                    concat!("control ", stringify!($subcommand),
                    " missing argument `", stringify!($var),"'"
                    )
                    )?;
                let (var, newins) = make_not_string(ctx, var,
                    // draw subcommand argument`var' cannot be a string
                    concat!("control ", stringify!($subcommand)," argument `",
                    stringify!($var),"' cannot be a string")
                    )?;
                ins.extend(newins);
                var
            };
        };
        ($subcommand:ident, $var:ident, $($vars:ident),+) => {
            notstr!($subcommand, $var);
            notstr!($subcommand, $($vars),+);
        };
    }

    let subcommand = match *subcmd {
        "enabled" => {
            notstr!(enabled, enabled);
            ControlSI::Enabled(enabled)
        },
        "shoot" => {
            notstr!(shoot, x, y, shoot);
            ControlSI::Shoot { x, y, shoot }
        },
        "shootp" => {
            let unit = argi.next().ok_or("control shootp missing argument `unit'")?;
            let unit = make_variable(
                expect_identifier(unit, "control shootp argument `unit' must be an identifier")?);
            notstr!(shootp, shoot);

            ControlSI::Shootp { unit, shoot }
        },
        "configure" => {
            let configuration = argi.next().ok_or("control configure missing argument `configuration'")?;
            let configuration = make_variable(
                expect_identifier(
                    configuration,
                    "control configure argument `configuration' must be an identifier")?
                );

            ControlSI::Configure(configuration)
        },
        "color" => {
            notstr!(color, r, g, b);
            ControlSI::Color{ r, g, b }
        },
        invalid => return Err(format!("Invalid control subcommand `{}'", invalid))
    };

    if argi.len() > 0 {
        return Err(format!("Extra arguments to control subcommand `{}'", *subcmd));
    }

    ins.push(Ins::Control{ target, subcommand });
    Ok(ins)
}

fn bind_function(ctx: Ctx) -> Result<Ins, String> {
    let args = ctx.args;
    if args.len() != 1 {
        return Err("bind expects exactly one argument".into());
    }

    let ident = expect_identifier(&args[0], "bind function's argument must be an identifier")?;
    if !ident.starts_with('@') {
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


/// uradar <sort> <order> <prop> <prop> <prop>
///
/// This function is basically just [`radar_function`]
/// without the `from` argument, but this is the easiest way to handle it.
/// TODO Maybe figure out a better way without duplicating code
fn uradar_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    use instructions::{ TargetProp, TargetSort };
    let Ctx { args, ret, .. } = ctx;

    if args.len() > 5 {
        return Err("Radar accepts 5 or less arguments".into());
    }

    let mut ins = Vec::new();
    let mut order: Arg = make_num(1);
    let mut sort: TargetSort = TargetSort::Distance;
    let mut conds: [TargetProp; 3] = [TargetProp::Any, TargetProp::Any, TargetProp::Any];

    if !args.is_empty() {
        let ident = expect_identifier(&args[1], "The `sort` argument to radar must be an identifier")?;
        sort = ident.parse().map_err(|_| "First argument to radar must be a valid sort")?;

        if args.len() >= 2 {
            let (neworder, newins) = make_not_string(ctx, &args[2], "Second argument to radar cannot be a string")?;
            order = neworder;
            ins.extend(newins);
        }

        if args.len() >= 3 {
            let ident = expect_identifier(&args[3], "The first condition argument to radar must be an identifier")?;
            conds[0] = ident.parse().map_err(|_| "Third argument to radar must be a valid condition")?;
        }

        if args.len() >= 4 {
            let ident = expect_identifier(&args[4], "The second condition argument to radar must be an identifier")?;
            conds[1] = ident.parse().map_err(|_| "Fourth argument to radar must be a valid condition")?;
        }

        if args.len() >= 5 {
            let ident = expect_identifier(&args[5], "The second condition argument to radar must be an identifier")?;
            conds[2] = ident.parse().map_err(|_| "Fifth argument to radar must be a valid condition")?;
        }
    }

    ins.push(Ins::UnitRadar {
        order,
        result: ret_or_null(ret),
        sort,
        conds
    });

    Ok(ins)
}

fn if_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    let Ctx { args, ret, .. } = ctx;

    let mut argi = args.iter().peekable();

    let mut next_label = generate_label(ctx);
    let end_label = generate_label(ctx);

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

                        next_label = generate_label(ctx);

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

fn draw_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    use instructions::DrawSI;
    let Ctx { args, .. } = ctx;
    let mut argi = args.iter();

    let mut ins = Vec::new();

    // A macro to make this function waaay shorter
    macro_rules! notstr {
        ($subcommand:ident, $var:ident) => {
            let $var = {
                let var = argi.next().ok_or(
                    // draw subcommand missing `var'
                    concat!("draw ", stringify!($subcommand),
                    " missing argument `", stringify!($var),"'"
                    )
                    )?;
                let (var, newins) = make_not_string(ctx, var,
                    // draw subcommand argument`var' cannot be a string
                    concat!("draw ", stringify!($subcommand)," argument `",
                    stringify!($var),"' cannot be a string")
                    )?;
                ins.extend(newins);
                var
            };
        };
        ($subcommand:ident, $var:ident, $($vars:ident),+) => {
            notstr!($subcommand, $var);
            notstr!($subcommand, $($vars),+);
        };
    }

    let subcommand = argi.next().ok_or("Draw function missing subcommand")?;
    let subcommand = expect_identifier(subcommand, "draw subcommand must be an identifier")?;
    let si: DrawSI = match *subcommand {
        "clear" => {
            notstr!(clear, r, g, b);
            DrawSI::Clear { r, g, b }
        },
        "color" => {
            notstr!(color, r, g, b, a);
            DrawSI::Color { r, g, b, a }
        },
        "stroke" => {
            notstr!(stroke, w);
            DrawSI::Stroke(w)
        },
        "line" => {
            notstr!(line, x1, y1, x2, y2);
            DrawSI::Line { x1, y1, x2, y2 }
        },
        "rect" => {
            notstr!(rect, x, y, width, height);
            DrawSI::Rect { x, y, width, height }
        },
        "linerect" => {
            notstr!(linerect, x, y, width, height);
            DrawSI::LineRect { x, y, width, height }
        },
        "poly" => {
            notstr!(poly, x, y, sides, radius, rotation);
            DrawSI::Poly { x, y, sides, radius, rotation }
        },
        "linepoly" => {
            notstr!(linepoly, x, y, sides, radius, rotation);
            DrawSI::LinePoly { x, y, sides, radius, rotation }
        },
        "triangle" => {
            notstr!(triangle, x1, y1, x2, y2, x3, y3);
            DrawSI::Triangle { x1, y1, x2, y2, x3, y3 }
        },
        "image" => {
            notstr!(image, x, y);
            let image = argi.next().ok_or("draw image missing argument `image'")?;
            let image = expect_identifier(image, "draw image argument `image' must be an identifier")?;
            if !image.starts_with('@') {
                warn!("draw image argument `image' should probably begin with a @");
            }
            let image = make_variable(image);
            notstr!(image, size, rotation);
            DrawSI::Image { x, y, image, size, rotation }
        }
        _ => return Err(format!("Unknown draw subcommand `{}'", *subcommand)),
    };

    if argi.len() > 0 {
        return Err(format!("Extra arguments to draw subcommand `{}'", *subcommand));
    }

    ins.push(Ins::Draw(si));

    Ok(ins)
}
