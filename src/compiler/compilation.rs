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
            Err(format!("{}", err))
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
                },
                DrawFlush(display) => format!("drawflush {}", disp!(display)),
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
                Noop => "noop".into(),
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
                Read { result, cell, at } => {
                    disp!([result, cell, at]);
                    format!("read {result} {cell} {at}")
                }
                Sensor { result: ret, target, sensable } => {
                    disp!([ret, target, sensable]);
                    format!("sensor {ret} {target} {sensable}")
                },
                Set { variable, value } => {
                    disp!([variable, value]);
                    format!("set {variable} {value}")
                },
                UnitBind(unit) => format!("ubind {}", disp!(unit)),
                UnitControl(subcmd) => {
                    use instructions::UnitControlSI::*;
                    match subcmd {
                        Idle => "ucontrol idle".into(),
                        Stop => "ucontrol stop".into(),
                        Move { x, y } => {
                            disp!([x, y]);
                            format!("ucontrol move {x} {y}")
                        },
                        Approach { x, y, radius } => {
                            disp!([x, y, radius]);
                            format!("ucontrol approach {x} {y} {radius}")
                        },
                        Boost(boost) => {
                            format!("ucontrol boost {}", disp!(boost))
                        },
                        Pathfind => "ucontrol pathfind".into(),
                        Target { x, y, shoot } => {
                            disp!([x, y, shoot]);
                            format!("ucontrol target {x} {y} {shoot}")
                        },
                        Targetp { unit, shoot } => {
                            disp!([unit, shoot]);
                            format!("ucontrol targetp {unit} {shoot}")
                        },
                        ItemDrop { to, amount } => {
                            disp!([to, amount]);
                            format!("ucontrol itemDrop {to} {amount}")
                        },
                        ItemTake { from, item, amount } => {
                            disp!([from, item, amount]);
                            format!("ucontrol itemTake {from} {item} {amount}")
                        },
                        PayDrop => "ucontrol payDrop".into(),
                        PayTake(takeunits) => format!("ucontrol payTake {}", disp!(takeunits)),
                        Mine { x, y } => {
                            disp!([x, y]);
                            format!("ucontrol mine {x} {y}")
                        },
                        Flag(value) => format!("ucontrol flag {}", disp!(value)),
                        Build { x, y, block, rotation, config } => {
                            disp!([x, y, block, rotation, config]);
                            format!("ucontrol build {x} {y} {block} {rotation} {config}")
                        },
                        GetBlock { x, y, building_type, building } => {
                            disp!([x, y, building_type, building]);
                            format!("ucontrol getBlock {x} {y} {building_type} {building}")
                        },
                        Within { x, y, radius, result } => {
                            disp!([x, y, radius, result]);
                            format!("ucontrol within {x} {y} {radius} {result}")
                        }
                    }
                }
                UnitLocate { found, outx, outy, subcommand } => {
                    use instructions::UnitLocateSI::*;
                    disp!([found, outx, outy]);
                    match subcommand {
                        Ore(ore) => format!("ulocate ore core null {} {outx} {outy} {found}", disp!(ore)),
                        Spawn(spawn) => format!("ulocate spawn core null null {outx} {outy} {found} {}", disp!(spawn)),
                        Damaged(damaged) => format!("ulocate damaged core null null {outx} {outy} {found} {}", disp!(damaged)),
                        Building { group, enemy, building } => {
                            disp!([enemy, building]);
                            let group: &'static str = (*group).into();
                            format!("ulocate building {group} {enemy} null {outx} {outy} {found} {building}")
                        }
                    }
                }
                UnitRadar { order, result: ret, sort, conds } => {
                    disp!([order, ret]);
                    let [cond1, cond2, cond3] = conds;
                    format!("uradar {cond1} {cond2} {cond3} {sort} 0 {order} {ret}")
                },
                Write { value, to, at } => {
                    disp!([value, to, at]);
                    format!("write {value} {to} {at}")
                },
                _Raw(s) => s.clone(),
                /* Never thought I'd see the day this line would be unnecesarry
                instruction => return Err(format!("Instruction {:?} not yet implemented for translation", instruction)),
                */
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

            if let Some(new_instruction) = combine_set_and_set(&ins[it], &ins[it + 1]) {
                res.push(new_instruction);
                it += 2;
                continue;
            }

            if let Some(new_instruction) = combine_read_and_set(&ins[it], &ins[it + 1]) {
                res.push(new_instruction);
                it += 2;
                continue;
            }

            if let Some(new_instruction) = combine_op_and_jump(&ins[it], &ins[it + 1]) {
                res.push(new_instruction);
                it += 2;
                continue;
            }

            if let Some(new_instruction) = combine_radar_and_set(&ins[it], &ins[it + 1]) {
                res.push(new_instruction);
                it += 2;
                continue;
            }

            if let Some(new_instruction) = combine_uradar_and_set(&ins[it], &ins[it + 1]) {
                res.push(new_instruction);
                it += 2;
                continue;
            }

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
        "drawflush" => ins.push(drawflush_function(newctx)?),
        "end" => ins.push(end_function(newctx)?),
        "getlink" => ins.extend(getlink_function(newctx)?),
        "if" => ins.extend(if_function(newctx)?),
        "iterlinks" => ins.extend(iterlinks_function(newctx)?),
        "noop" => ins.push(noop_function(newctx)?),
        "printflush" => ins.push(printflush_function(newctx)?),
        "radar" => ins.extend(radar_function(newctx)?),
        "print" => {
            let (mut i, [a]) = generic_passthrough::<1>("print", newctx)?;
            i.push(Ins::Print(a));
            ins.extend(i);
        },
        "println" => {
            let (mut i, [a]) = generic_passthrough::<1>("println", newctx)?;
            i.push(Ins::Print(a));
            i.push(Ins::Print(newline(ctx)));
            ins.extend(i);
        },
        "read" => ins.extend(read_function(newctx)?),
        "set" => ins.extend(set_function(newctx)?),
        "sensor" => ins.push(sensor_function(newctx)?),
        "sleep" => ins.extend(sleep_function(newctx)?),
        "ucontrol" => ins.extend(ucontrol_function(newctx)?),
        "ulocate" => ins.extend(ulocate_function(newctx)?),
        "uradar" => ins.extend(uradar_function(newctx)?),
        "while" => ins.extend(while_function(newctx)?),
        "write" => ins.extend(write_function(newctx)?),
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

    let ident = expect_identifier(&args[0], "set argument `variable' must be an identifier")?;

    let (value, mut ins) = make_generic(ctx, &args[1])?;
    let var = make_variable(ident);
    ins.push(Ins::Set { variable: var.clone(), value });
    ins.push(Ins::Set { variable: ret_or_null(ctx, ret), value: var });

    Ok(ins)
}

fn do_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    let Ctx { args, .. } = ctx;
    let mut ins = Vec::new();
    for arg in args {
        if let Argument::Statement(stmnt) = arg {
            let newins = compile_statement(stmnt, ctx.no_ret())?;
            ins.extend(newins);
        } else {
            return Err("do function only accepts statements as arguments".into());
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

    ins.push(Ins::Op { op, result: ret_or_null(ctx, ret), left, right });

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
    ins.push(Ins::GetLink{store: ret_or_null(ctx, ret), index: arg});
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
    ins.push(Ins::GetLink { store: str_to_var(ctx, "link"), index: itervar.clone() });
    ins.push(Ins::Jump {
        label: exit_label,
        cmp: Comparison::StrictEquals,
        left: itervar.clone(),
        right: str_to_var(ctx, "null")
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
        right: str_to_var(ctx, "@links")
    });
    ins.push(Ins::Label(exit_label));

    Ok(ins)
}

fn printflush_function(ctx: Ctx) -> Result<Ins, String> {
    let Ctx { args, .. } = ctx;
    match args.len() {
        0 => Ok(Ins::PrintFlush(str_to_var(ctx, "message1"))),
        1 => {
            let message = expect_identifier(&args[0], "printflush argument `message' must be an identifier")?;
            Ok(Ins::DrawFlush(make_variable(message)))
        },
        count => Err(format!("printflush accepts no arguments or one argument, not {count}"))
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
    Ok(Ins::Sensor { result: ret_or_null(ctx, ret), target, sensable })
}

fn control_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    use instructions::ControlSI;
    let Ctx { args, .. } = ctx;

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
        return Err(
            format!("{} extra argument{} to control subcommand `{}'",
                argi.len(), if argi.len() > 1 { "s" } else { "" }, *subcmd)
            );
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
    let mut from: Arg = str_to_var(ctx, "@this");
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
            let ident = expect_identifier(&args[5], "The third condition argument to radar must be an identifier")?;
            conds[2] = ident.parse().map_err(|_| "Sixth argument to radar must be a valid condition")?;
        }
    }

    ins.push(Ins::Radar {
        from,
        order,
        result: ret_or_null(ctx, ret),
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
        let ident = expect_identifier(&args[0], "The `sort` argument to radar must be an identifier")?;
        sort = ident.parse().map_err(|_| "First argument to radar must be a valid sort")?;

        if args.len() >= 2 {
            let (neworder, newins) = make_not_string(ctx, &args[1], "Second argument to radar cannot be a string")?;
            order = neworder;
            ins.extend(newins);
        }

        if args.len() >= 3 {
            let ident = expect_identifier(&args[2], "The first condition argument to radar must be an identifier")?;
            conds[0] = ident.parse().map_err(|_| "Third argument to radar must be a valid condition")?;
        }

        if args.len() >= 4 {
            let ident = expect_identifier(&args[3], "The second condition argument to radar must be an identifier")?;
            conds[1] = ident.parse().map_err(|_| "Fourth argument to radar must be a valid condition")?;
        }

        if args.len() >= 5 {
            let ident = expect_identifier(&args[4], "The second condition argument to radar must be an identifier")?;
            conds[2] = ident.parse().map_err(|_| "Fifth argument to radar must be a valid condition")?;
        }
    }

    ins.push(Ins::UnitRadar {
        order,
        result: ret_or_null(ctx, ret),
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
    ins.push(Ins::Set { variable: ret_or_null(ctx, ret), value: bodyresult });

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
                        ins.push(Ins::Set { variable: ret_or_null(ctx, ret), value: bodyresult });
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
        ins.push(Ins::Set { variable: ret_or_null(ctx, ret), value: bodyresult });
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

    let subcommand = argi.next().ok_or("draw function missing subcommand")?;
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
            notstr!(stroke, stroke);
            DrawSI::Stroke(stroke)
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
                warn!("draw image argument `image' should probably begin with an @");
            }
            let image = make_variable(image);
            notstr!(image, size, rotation);
            DrawSI::Image { x, y, image, size, rotation }
        }
        _ => return Err(format!("Unknown draw subcommand `{}'", *subcommand)),
    };

    if argi.len() > 0 {
        return Err(
            format!("{} extra argument{} to control subcommand `{}'",
                argi.len(), if argi.len() > 1 { "s" } else { "" }, *subcommand)
            );
    }

    ins.push(Ins::Draw(si));

    Ok(ins)
}

fn ulocate_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    use instructions::UnitLocateSI;
    let Ctx { args, ret, .. } = ctx;
    let mut argi = args.iter();

    let mut ins = Vec::new();

    let subcommand = argi.next().ok_or("ulocate function missing subcommand")?;
    let subcommand = expect_identifier(subcommand, "ulocate subcommand must be an identifier")?;

    let outx = argi.next().ok_or(format!("ulocate {} missing argument `outx'", *subcommand))?;
    let outx = expect_identifier(outx, &format!("ulocate {} argument `outx' must be an identifier", *subcommand))?;
    let outx = make_variable(outx);
    let outy = argi.next().ok_or(format!("ulocate  {} missing argument `outy'", *subcommand))?;
    let outy = expect_identifier(outy, &format!("ulocate {} argument `outy' must be an identifier", *subcommand))?;
    let outy = make_variable(outy);

    let subcmd: UnitLocateSI = match *subcommand {
        "ore" => {
            let ore = argi.next().ok_or("ulocate ore missing argument `ore'")?;
            let ore = expect_identifier(ore, "ulocate ore argument `ore' must be an identifier")?;
            if !ore.starts_with('@') {
                warn!("ulocate ore argument `ore' should probably begin with an @");
            }
            let ore = make_variable(ore);

            UnitLocateSI::Ore(ore)
        },
        "spawn" => {
            let spawn = argi.next().ok_or("ulocate spawn missing argument `spawn'")?;
            let spawn = expect_identifier(spawn, "ulocate spawn argument `spawn' must be an identifier")?;
            let spawn = make_variable(spawn);
            UnitLocateSI::Spawn(spawn)
        },
        "damaged" => {
            let damaged = argi.next().ok_or("ulocate damaged missing argument `damaged'")?;
            let damaged = expect_identifier(damaged, "ulocate damaged argument `damaged' must be an identifier")?;
            let damaged = make_variable(damaged);

            UnitLocateSI::Damaged(damaged)
        },
        "building" => {
            let group = argi.next().ok_or("ulocate building missing argument `group'")?;
            let group = expect_identifier(group, "ulocate building argument `group' must be an identifier")?;
            let group = group.parse().map_err(|_| "ulocate building argument `group' isn't a valid building group")?;

            let enemy = argi.next().ok_or("ulocate building missing argument `enemy'")?;
            let (enemy, newins) = make_not_string(ctx, enemy, "ulocate building argument `enemy' cannot be a string")?;
            ins.extend(newins);

            let building = argi.next().ok_or("ulocate building missing argument `building'")?;
            let building = expect_identifier(building, "ulocate building argument `building' must be an identifier")?;
            let building = make_variable(building);

            UnitLocateSI::Building { group, enemy, building }
        },
        invalid => return Err(format!("Unknown ulocate subcommand `{invalid}'")),
    };

    if argi.len() > 0 {
        return Err(
            format!("{} extra argument{} to ulocate subcommand `{}'",
                argi.len(), if argi.len() > 1 { "s" } else { "" }, *subcommand)
            );
    }

    ins.push(Ins::UnitLocate { outx, outy, found: ret_or_null(ctx, ret), subcommand: subcmd });
    Ok(ins)
}

fn ucontrol_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    use instructions::UnitControlSI;
    let Ctx { args, .. } = ctx;
    let mut argi = args.iter();

    let mut ins = Vec::new();
    let subcommand = argi.next().ok_or("ulocate function missing subcommand")?;
    let subcommand = expect_identifier(subcommand, "ulocate subcommand must be an identifier")?;

    // Utility macros
    macro_rules! notstr {
        ($subcommand:ident, $var:ident) => {
            let $var = {
                let var = argi.next().ok_or(
                    // draw subcommand missing `var'
                    concat!("ucontrol ", stringify!($subcommand),
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

    macro_rules! ident {
        ($subcommand:ident, $var:ident) => {
            let $var = {
                let next = argi.next().ok_or(
                    concat!(
                        "draw ", stringify!($subcommand),
                        " missing argument `", stringify!($var), "'"
                    )
                )?;
                make_variable(
                    expect_identifier(next,
                        concat!(
                            "draw ", stringify!($subcommand),
                            " argument `", stringify!($var), "' must be an identifier"
                        )
                    )?
                )
            };
        };
        ($subcommand:ident, $var:ident, $($vars:ident),+) => {
            ident!($subcommand, $var);
            ident!($subcommand, $($vars),+);
        };
    }

    macro_rules! atident {
        ($subcommand:ident, $var:ident) => {
            let $var = {
                let next = argi.next().ok_or(
                    concat!(
                        "draw ", stringify!($subcommand),
                        " missing argument `", stringify!($var), "'"
                    )
                )?;
                let ident = expect_identifier(next,
                    concat!(
                        "draw ", stringify!($subcommand),
                        " argument `", stringify!($var), "' must be an identifier"
                        )
                    )?;
                if !ident.starts_with('@') {
                    log::warn!(concat!(
                        "draw ", stringify!($subcommand),
                        " argument `", stringify!($var), "' should probably start with an @"
                    ))
                }
                make_variable(ident)
            };
        };
        ($subcommand:ident, $var:ident, $($vars:ident),+) => {
            atident!($subcommand, $var);
            atident!($subcommand, $($vars),+);
        };
    }

    let subcmd: UnitControlSI = match *subcommand {
        "idle" => UnitControlSI::Idle,
        "stop" => UnitControlSI::Stop,
        "move" => {
            // Lets hope this won't blow up T_T
            notstr!(move, x, y);
            UnitControlSI::Move { x, y }
        },
        "approach" => {
            notstr!(approach, x, y, radius);
            UnitControlSI::Approach { x, y, radius }
        },
        "boost" => {
            notstr!(boost, boost);
            UnitControlSI::Boost(boost)
        },
        "pathfind" => UnitControlSI::Pathfind,
        "target" => {
            notstr!(target, x, y, shoot);
            UnitControlSI::Target { x, y, shoot }
        },
        "targetp" => {
            ident!(targetp, unit);
            notstr!(targetp, shoot);
            UnitControlSI::Targetp { unit, shoot }
        },
        "itemdrop" => {
            ident!(itemdrop, to);
            notstr!(itemdrop, amount);
            UnitControlSI::ItemDrop { to, amount }
        },
        "itemtake" => {
            ident!(itemtake, from);
            atident!(itetake, item);
            notstr!(itemtake, amount);
            UnitControlSI::ItemTake { from, item, amount }
        },
        "paydrop" => UnitControlSI::PayDrop,
        "paytake" => {
            notstr!(paytake, takeunits);
            UnitControlSI::PayTake(takeunits)
        },
        "mine" => {
            notstr!(mine, x, y);
            UnitControlSI::Mine { x, y }
        },
        "flag" => {
            notstr!(flag, value);
            UnitControlSI::Flag(value)
        },
        "build" => {
            notstr!(build, x, y);
            atident!(build, block);
            notstr!(build, rotation);
            ident!(build, config);
            UnitControlSI::Build { x, y, block, rotation, config }
        },
        "getblock" => {
            notstr!(getblock, x, y);
            ident!(getblock, building_type, building);
            UnitControlSI::GetBlock { x, y, building_type, building }
        },
        "within" => {
            notstr!(within, x, y, radius);
            ident!(within, result);
            UnitControlSI::Within { x, y, radius, result }
        },
        invalid => return Err(format!("Unknown ucontrol subcommand `{invalid}'")),
    };

    if argi.len() > 0 {
        return Err(
            format!("{} extra argument{} to ucontrol subcommand `{}'",
                argi.len(), if argi.len() > 1 { "s" } else { "" }, *subcommand)
            );
    }

    ins.push(Ins::UnitControl(subcmd));
    Ok(ins)
}

fn read_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    let Ctx { args, ret, .. } = ctx;
    let mut argi = args.iter();
    let mut ins = Vec::new();

    let cell = argi.next().ok_or("read missing argument `cell'")?;
    let cell = make_variable(expect_identifier(cell, "read argument `cell' must be an identifier")?);

    let at = argi.next().ok_or("read missing argument `at'")?;
    let (at, newins) = make_not_string(ctx, at, "read argument `at' cannot be a string")?;
    ins.extend(newins);

    if argi.len() > 0 {
        return Err(
            format!("{} extra argument{} to read",
                argi.len(), if argi.len() > 1 { "s" } else { "" })
            );
    }

    ins.push(Ins::Read { at, cell, result: ret_or_null(ctx, ret) });

    Ok(ins)
}

fn write_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    let Ctx { args, ret, .. } = ctx;
    let mut argi = args.iter();
    let mut ins = Vec::new();

    let to = argi.next().ok_or("read missing argument `to'")?;
    let to = make_variable(expect_identifier(to, "write argument `to' must be an identifier")?);

    let at = argi.next().ok_or("read missing argument `at'")?;
    let (at, newins) = make_not_string(ctx, at, "write argument `at' cannot be a string")?;
    ins.extend(newins);

    let value = argi.next().ok_or("read missing argument `value'")?;
    let (value, newins) = make_not_string(ctx, value, "write argument `value' cannot be a string")?;
    ins.extend(newins);

    if argi.len() > 0 {
        return Err(
            format!("{} extra argument{} to write",
                argi.len(), if argi.len() > 1 { "s" } else { "" })
            );
    }

    ins.push(Ins::Write { to, at, value: value.clone() });
    ins.push(Ins::Set { variable: ret_or_null(ctx, ret), value });

    Ok(ins)
}

fn end_function(ctx: Ctx) -> Result<Ins, String> {
    let Ctx { args, .. } = ctx;

    if !args.is_empty() {
        return Err("end function accepts no arguments".into());
    }

    Ok(Ins::End)
}

fn noop_function(ctx: Ctx) -> Result<Ins, String> {
    let Ctx { args, .. } = ctx;

    if !args.is_empty() {
        return Err("noop function accepts no arguments".into());
    }

    Ok(Ins::Noop)
}

fn drawflush_function(ctx: Ctx) -> Result<Ins, String> {
    let Ctx { args, .. } = ctx;
    match args.len() {
        0 => Ok(Ins::DrawFlush(str_to_var(ctx, "display1"))),
        1 => {
            let display = expect_identifier(&args[0], "drawflush argument must be an identifier")?;
            Ok(Ins::DrawFlush(make_variable(display)))
        },
        count => Err(format!("drawflush accepts no arguments or one argument, not {count}"))
    }
}

fn sleep_function(ctx: Ctx) -> Result<Vec<Ins>, String> {
    let Ctx { args, .. } = ctx;

    if args.len() != 1 {
        return Err("sleep function accepts exactly one argument".into());
    }

    let mut ins = Vec::with_capacity(5);

    let (duration, newins) = make_not_string(ctx, &args[0], "sleep argument `duration' cannot be a string")?;
    let additional_time: u16 = newins.iter().fold(0, |a, i| a.saturating_add(i.size().try_into().unwrap()));
    ins.extend(newins);

    let end_time = generate_variable(ctx);
    ins.push(Ins::Op {
        op: Operation::Mult,
        result: end_time.clone(),
        left: make_num(1000),
        right: duration,
    });
    ins.push(Ins::Op {
        op: Operation::Plus,
        result: end_time.clone(),
        left: str_to_var(ctx, "@time"),
        right: end_time.clone(),
    });
    ins.push(Ins::Op {
        op: Operation::Minus,
        result: end_time.clone(),
        left: end_time.clone(),
        right: make_num::<u16>(3 + additional_time),
    });

    let repeat_label = generate_label(ctx);
    ins.push(Ins::Label(repeat_label));
    ins.push(Ins::Jump {
        cmp: Comparison::LessThan,
        label: repeat_label,
        left: str_to_var(ctx, "@time"),
        right: end_time,
    });

    Ok(ins)
}
