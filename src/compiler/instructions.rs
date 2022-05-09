use std::{
    str::FromStr,
    borrow::Cow,
    collections::HashMap,
};
use rand::rngs::StdRng;
use super::{
    settings::Settings,
};

#[derive(Debug, Clone, PartialEq)]
pub enum Arg {
    Literal(Type),
    Variable(Vartype)
}

/// Type of variable
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Vartype {
    Named(String),
    Unnamed(usize)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Num(f64),
    Str(String)
}

type Label = usize;

#[derive(Debug, Clone, PartialEq)]
pub enum Ins {
    /// read store cell address
    Read([Arg; 3]),
    /// write value, cell, address
    Write([Arg; 3]),
    /// draw subcommand ...
    Draw(DrawSI),
    /// print text
    Print(Arg),
    /// drawflush display
    DrawFlush(Arg),
    /// printflush message
    PrintFlush(Arg),
    /// getlink store, index
    GetLink { store: Arg, index: Arg },
    /// control subcommand target
    /*       v-- target*/
    Control {
        target: Arg,
        subcommand: ControlSI
    },
    /// radar from order result sort prop prop prop
    Radar {
        /// Bulding from which we're searching
        from: Arg,
        order: Arg,
        result: Arg,
        sort: TargetSort,
        conds: [TargetProp; 3]
    },
    /// sensor result target sensable
    Sensor {
        result: Arg,
        target: Arg,
        sensable: Arg
    },
    /// set variable value
    Set {
        variable: Arg,
        value: Arg
    },
    /// op operation result left right
    Op {
        op: Operation,
        result: Arg,
        left: Arg,
        right: Arg
    },
    /// end
    End,
    /// jump addr comp left right
    Jump {
        label: Label,
        cmp: Comparison,
        left: Arg,
        right: Arg
    },
    /// ubind type
    UnitBind(Arg),
    /// ucontrol subcommand ...
    UnitControl(UnitControlSI),
    /// uradar prop prop prop outX outY outFound
    UnitRadar {
        order: Arg,
        result: Arg,
        sort: TargetSort,
        conds: [TargetProp; 3]
    },
    /// ulocate subcommand ...
    UnitLocate(UnitLocateSI),
    /// noop
    Noop,

    /*
     *   Onwards are pseudo-instructions, which do not exist
     *   in mlog, and are used as a crutch by slimlog
     */

    Label(Label),
    /// Passed-through exactly as it is
    _Raw(String)
}

#[derive(Debug, Clone, PartialEq)]
pub enum DrawSI {
    /// clear r, g, b
    Clear([Arg; 3]),
    /// color r, g, b
    Color([Arg; 3]),
    /// stroke width
    Stroke(Arg),
    /// line x1, y1, x2, y2
    Line([Arg; 4]),
    /// rect x, y, w, h
    Rect([Arg; 4]),
    /// lineRect x, y, w, h
    LineRect([Arg; 4]),
    /// poly x, y, sides, radius, rotation
    Poly([Arg; 5]),
    /// linePoly x, y, sides, radius, rotation
    LinePoly([Arg; 5]),
    /// triangle x1, y1, x2, y2, x3, y3
    Triangle([Arg; 6]),
    /// image x, y, image, size, rotation
    Image([Arg; 4])
}

#[derive(Debug, Clone, PartialEq)]
pub enum ControlSI {
    /// enabled target enabled
    Enabled(Arg),
    /// shoot turret, x, y, shoot
    Shoot {
        x: Arg,
        y: Arg,
        shoot: Arg
    },
    /// shootp turret, unit, shoot
    Shootp {
        unit: Arg,
        shoot: Arg
    },
    /// configure building configuration
    Configure(Arg),
    /// color illuminator r g b
    Color {
        r: Arg,
        g: Arg,
        b: Arg
    }
}

/// Target Property
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TargetProp {
    Any,
    Enemy,
    Ally,
    Player,
    Attacker,
    Flying,
    Boss,
    Ground
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TargetSort {
    Distance,
    Health,
    Shield,
    Armor,
    MaxHealth
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Operation {
    Plus,
    Minus,
    Mult,
    Div,
    IDiv,
    Modulo,
    Power,
    Equals,
    NotEquals,
    And,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    StrictEquals,
    LeftShift,
    RightShift,
    Or,
    BinaryAnd,
    Xor,
    Flip,
    Max,
    Min,
    Angle,
    Len,
    Noise,
    Abs,
    Log,
    Log10,
    Sin,
    Cos,
    Tan,
    Floor,
    Ceil,
    Sqrt,
    Rand
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Comparison {
    Equals,
    NotEquals,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    StrictEquals,
    Always
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnitControlSI {
    Idle,
    Stop,
    Move([Arg; 2]),
    Approach([Arg; 3]),
    Boost(Arg),
    Pathfind,
    Target([Arg; 3]),
    Targetp([Arg; 2]),
    ItemDrop([Arg; 2]),
    ItemTake([Arg; 3]),
    PayDrop,
    PayTake(Arg),
    Mine([Arg; 2]),
    Flag(Arg),
    Build([Arg; 5]),
    GetBlock([Arg; 4]),
    Within([Arg; 4])
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnitLocateSI {
    Ore([Arg; 4]),
    Building(BuildingGroup, [Arg; 5]),
    Spawn([Arg; 4]),
    Damaged([Arg; 4])
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum BuildingGroup {
    Core,
    Storage,
    Generator,
    Turret,
    Factory,
    Repair,
    Rally,
    Battery,
    Resupply,
    Reactor
}

/// Panics when fails, for non-panicking version, check out TryFrom<Operation> for String
impl ToString for Operation {
    fn to_string(&self) -> String {
        let res: &str = (*self).into();
        res.to_string()
    }
}

impl From<Operation> for &'static str {
    fn from(op: Operation) -> Self {
        use Operation::*;
        match op {
            Plus => "add",
            Minus => "sub",
            Mult => "mul",
            Div => "div",
            IDiv => "idiv",
            Modulo => "mod",
            Power => "pow",
            Equals => "equal",
            NotEquals => "notEqual",
            And => "land",
            LessThan => "lessThan",
            LessOrEqual => "lessThanEq",
            GreaterThan => "greaterThan",
            GreaterOrEqual => "greaterThanEq",
            StrictEquals => "strictEqual",
            LeftShift => "shl",
            RightShift => "shr",
            Or => "or",
            BinaryAnd => "and",
            Xor => "xor",
            Flip => "not",
            Max => "max",
            Min => "min",
            Angle => "angle",
            Len => "len",
            Noise => "noise",
            Abs => "abs",
            Log => "log",
            Log10 => "log10",
            Sin => "sin",
            Cos => "cos",
            Tan => "tan",
            Floor => "floor",
            Ceil => "ceil",
            Sqrt => "sqrt",
            Rand => "rand"
        }
    }
}

impl FromStr for Operation {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Operation::*;
        Ok(match s {
            "+" => Plus,
            "-" => Minus,
            "*" => Mult,
            "/" => Div,
            "//" => IDiv,
            "%" => Modulo,
            "**" => Power,
            "==" => Equals,
            "!=" => NotEquals,
            "&&" => And,
            "<" => LessThan,
            "<=" => LessOrEqual,
            ">" => GreaterThan,
            ">=" => GreaterOrEqual,
            "===" => StrictEquals,
            "<<" => LeftShift,
            ">>" => RightShift,
            "|" => Or,
            "&" => BinaryAnd,
            "^" => Xor,
            "~" => Flip,
            "max" => Max,
            "min" => Min,
            "angle" => Angle,
            "len" => Len,
            "noise" => Noise,
            "abs" => Abs,
            "log" => Log,
            "log10" => Log10,
            "sin" => Sin,
            "cos" => Cos,
            "tan" => Tan,
            "floor" => Floor,
            "ceil" => Ceil,
            "sqrt" => Sqrt,
            "rand" => Rand,
            _ => return Err(())
        })
    }
}

impl Operation {
    /// Returns true if operation is unary
    pub fn unary(self) -> bool {
        use Operation::*;
        matches!(self, Flip | Abs | Log | Log10 | Sin | Cos | Tan | Floor | Ceil | Sqrt | Rand)
    }
}

impl TryFrom<Operation> for Comparison {
    type Error = ();
    fn try_from(op: Operation) -> Result<Self, Self::Error> {
        Ok(match op {
            Operation::Equals => Comparison::Equals,
            Operation::NotEquals => Comparison::NotEquals,
            Operation::LessThan => Comparison::LessThan,
            Operation::LessOrEqual => Comparison::LessOrEqual,
            Operation::GreaterThan => Comparison::GreaterThan,
            Operation::GreaterOrEqual => Comparison::GreaterOrEqual,
            Operation::StrictEquals => Comparison::StrictEquals,
            _ => return Err(())
        })
    }
}

impl TryFrom<Comparison> for Operation {
    type Error = ();
    fn try_from(op: Comparison) -> Result<Self, Self::Error> {
        Ok(match op {
            Comparison::Equals => Operation::Equals,
            Comparison::NotEquals => Operation::NotEquals,
            Comparison::LessThan => Operation::LessThan,
            Comparison::LessOrEqual => Operation::LessOrEqual,
            Comparison::GreaterThan => Operation::GreaterThan,
            Comparison::GreaterOrEqual => Operation::GreaterOrEqual,
            Comparison::StrictEquals => Operation::StrictEquals,
            _ => return Err(())
        })
    }
}

impl Ins {

    /// Returns an instruction's size in lines
    pub fn size(&self) -> usize {
        use Ins::*;
        match self {
            Label(_) => 0,
            _ => 1
        }
    }

    /// Compares two instructions solely based on variant
    pub fn cmp(&self, other: &Self) -> bool {
        use std::mem::discriminant;
        discriminant(self) == discriminant(other)
    }
}

impl From<Comparison> for &'static str {
    fn from(cmp: Comparison) -> Self {
        use Comparison::*;
        match cmp {
            Equals => "equal",
            NotEquals => "notEqual",
            LessThan => "lessThan",
            LessOrEqual => "lessThanEq",
            GreaterThan => "greaterThan",
            GreaterOrEqual => "greaterThanEq",
            StrictEquals => "strictEqual",
            Always => "always"
        }
    }
}

impl Arg {

    /// Compares Argument based on Arg and Type
    pub fn cmp(&self, other: &Self) -> bool {
        use std::mem::discriminant;
        match self {
            Arg::Variable(_) => discriminant(self) == discriminant(other),
            Arg::Literal(stype) => {
                match other {
                    Arg::Variable(_) => false,
                    Arg::Literal(otype) => {
                        stype.cmp(otype)
                    }
                }
            }
        }
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Arg::Variable(_))
    }

    /// Displays an Arg
    pub fn display<'a, 't>(
        &'a self,
        rng: &'t mut StdRng,
        map: &'t mut HashMap<usize, String>,
        settings: &'t Settings)
        -> Cow<'a, str>
    {
        match self {
            Arg::Literal(t) => {
                match t {
                    Type::Num(f) => {
                        Cow::Owned(f.to_string())
                    },
                    Type::Str(s) => Cow::Borrowed(s)
                }
            },
            Arg::Variable(v) => match v {
                Vartype::Named(s) => Cow::Borrowed(s),
                Vartype::Unnamed(n) => {
                    if settings.get_hex_unnamed_vars() {
                        if map.contains_key(n) {
                            return Cow::Owned(map[n].clone());
                        }

                        use rand::Rng;

                        let varlen = settings.get_hex_var_length();

                        const CHARSET: &[u8] = b"0123456789abcdef";
                        let mut var = String::with_capacity(2 + varlen as usize);
                        var.push_str("__");

                        for _ in 0..varlen {
                            var.push(CHARSET[rng.gen_range(0..CHARSET.len())] as char)
                        }

                        map.insert(*n, var.clone());

                        Cow::Owned(var)
                    } else {
                        Cow::Owned(format!("__{}", n))
                    }
                }
            }
        }
    }
}

impl Type {

    /// Compares Type based on enum variant
    pub fn cmp(&self, other: &Self) -> bool {
        use std::mem::discriminant;
        discriminant(self) == discriminant(other)
    }
}

impl Comparison {
    /// Inverts Comparison into it's opposite
    pub fn negate(self) -> Option<Self> {
        use Comparison::*;
        Some(match self {
            Equals => NotEquals,
            NotEquals => Equals,
            LessThan => GreaterOrEqual,
            LessOrEqual => GreaterThan,
            GreaterThan => LessOrEqual,
            GreaterOrEqual => LessThan,
            StrictEquals => {
                log::warn!(
                    concat!("Negating StrictEquals into NotEquals may cause",
                        "unintented consequences. This may have been invoked by an optimization")
                    );
                NotEquals
            },
            Always => return None
        })
    }
}

impl FromStr for TargetSort {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use TargetSort::*;
        Ok(match s {
            "armor" => Armor,
            "distance" => Distance,
            "health" => Health,
            "maxhealth" => MaxHealth,
            "shield" => Shield,
            _ => return Err(())
        })
    }
}

impl From<TargetSort> for &'static str {
    fn from(sort: TargetSort) -> Self {
        use TargetSort::*;
        match sort {
            Distance => "distance",
            Health => "health",
            Shield => "shield",
            Armor => "armor",
            MaxHealth => "maxHealth"
        }
    }
}

impl std::fmt::Display for TargetSort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str((*self).into()).expect("Failed to write to Formatter");
        Ok(())
    }
}

impl FromStr for TargetProp {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use TargetProp::*;
        Ok(match s {
            "any" => Any,
            "enemy" => Enemy,
            "ally" => Ally,
            "player" => Player,
            "attacker" => Attacker,
            "flying" => Flying,
            "boss" => Boss,
            "ground" => Ground,
            _ => return Err(())
        })
    }
}

impl From<TargetProp> for &'static str {
    fn from(prop: TargetProp) -> Self {
        use TargetProp::*;
        match prop {
            Any => "any",
            Enemy => "enemy",
            Ally => "ally",
            Player => "player",
            Attacker => "attacker",
            Flying => "flying",
            Boss => "boss",
            Ground => "ground"
        }
    }
}

impl std::fmt::Display for TargetProp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.write_str((*self).into()).expect("Failed to write to Formatter");
        Ok(())
    }
}

impl From<Vartype> for Arg {
    fn from(var: Vartype) -> Self {
        Arg::Variable(var)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn type_cmp() {
        use crate::compiler::instructions::{
            Arg::*, Type::*, Vartype
        };
        assert!(Literal(Num(5.0)).cmp(&Literal(Num(10.0))));
        assert!(Literal(Str("Pain".into())).cmp(&Literal(Str("Suffering".into()))));
        assert!(Variable(Vartype::Named("Pain".into())).cmp(&Variable(Vartype::Named("Suffering".into()))));
    }
}
