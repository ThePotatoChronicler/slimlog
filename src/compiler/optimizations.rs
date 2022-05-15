/* Optimizations used during translation
 */

use super::{
    instructions::{
        Ins,
        Arg,
        Vartype,
        Type,
        Comparison,
    },
};

macro_rules! clones {
    ($var:ident) => {
        let $var = $var.clone();
    };
    ($var:ident, $($vars:ident),+) => {
        clones!($var);
        clones!($($vars),+);
    }
}

/// Combines two set operations if the result of first
/// is the value of the second, and they're temporary variables
pub(crate) fn combine_set_and_set(ins1: &Ins, ins2: &Ins) -> Option<Ins> {
    match (ins1, ins2) {
        ( Ins::Set { variable: Arg::Variable(Vartype::Unnamed(var1)), value: val1 },
          Ins::Set { variable: var2, value: Arg::Variable(Vartype::Unnamed(val2)) }) => {
            if var1 == val2 {
                Some(Ins::Set { variable: var2.clone(), value: val1.clone() })
            } else {
                None
            }
        },
        _ => None
    }
}

pub(crate) fn check_null_set_or_op(ins: &Ins) -> bool {
    match ins {
        Ins::Set { variable: Arg::Variable(Vartype::Named(var)), .. } => var == "null",
        Ins::Op { result: Arg::Variable(Vartype::Named(var)), .. } => var == "null",
        _ => false
    }
}

pub(crate) fn combine_op_and_jump(ins1: &Ins, ins2: &Ins) -> Option<Ins> {
    match (ins1, ins2) {
        ( Ins::Op { op: opr, result, left: oarg0, right: oarg1 },
          Ins::Jump { label, cmp, left: jarg0, right: jarg1 } ) => {
            if !matches!(cmp, Comparison::StrictEquals) {
                return None
            }

            // Make sure that we're not deleting an op with a named result
            /*
            if !matches!(result, Arg::Variable(Vartype::Unnamed(_))) {
                return None;
            }
            */

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
                                cmp: opr_as_cmp.negate().unwrap(),
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
                                cmp: opr_as_cmp.negate().unwrap(),
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

/// Used to combine `set foo (+ 5 10)`
pub(crate) fn combine_op_and_set(ins1: &Ins, ins2: &Ins) -> Option<Ins> {
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

pub(crate) fn combine_radar_and_set(ins1: &Ins, ins2: &Ins) -> Option<Ins> {
    match (ins1, ins2) {
        ( Ins::Radar { result: Arg::Variable(Vartype::Unnamed(resvar)), .. },
          Ins::Set { variable, value: Arg::Variable(Vartype::Unnamed(setvalue)) }) => {
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

pub(crate) fn combine_uradar_and_set(ins1: &Ins, ins2: &Ins) -> Option<Ins> {
    match (ins1, ins2) {
        ( Ins::UnitRadar { result: Arg::Variable(Vartype::Unnamed(resvar)), .. },
          Ins::Set { variable, value: Arg::Variable(Vartype::Unnamed(setvalue)) }) => {
            if resvar == setvalue {
                let mut new_ins: Ins = ins1.clone();
                match new_ins {
                    Ins::UnitRadar { ref mut result, .. } => *result = variable.clone(),
                    _ => unreachable!()
                };
                return Some(new_ins);
            }
        },
        _ => return None
    };
    None
}

pub(crate) fn combine_read_and_set(ins1: &Ins, ins2: &Ins) -> Option<Ins> {
    match (ins1, ins2) {
        ( Ins::Read { result: Arg::Variable(Vartype::Unnamed(result)), cell, at },
          Ins::Set { variable, value: Arg::Variable(Vartype::Unnamed(setvalue)) }) => {
            if result != setvalue {
                return None;
            }
            clones!(variable, cell, at);
            Some(Ins::Read { result: variable, cell, at})
        },
        _ => None
    }
}
