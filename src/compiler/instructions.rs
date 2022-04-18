#[derive(Debug, Clone, PartialEq)]
pub enum Arg {
    Literal(Type),
    Variable(String)
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
    GetLink([Arg; 2]),
    /// control subcommand target
    /*       v-- target*/
    Control(Arg, ControlSI),
    /// radar prop prop prop building order result
    Radar([TargetProp; 3], [Arg; 3]),
    /// sensor store block sensable
    Sensor([Arg; 3]),
    /// set variable value
    Set([Arg; 2]),
    /// op operation result left right
    Op(Operation, [Arg; 3]),
    /// end
    End,
    /// jump addr comp left right
    Jump(Label, Comparison, [Arg; 2]),
    /// ubind type
    UnitBind(Arg),
    /// ucontrol subcommand ...
    UnitControl(UnitControlSI),
    /// uradar prop prop prop outX outY outFound
    UnitRadar([TargetProp; 3], [Arg; 3]),
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
    Shoot([Arg; 3]),
    /// shootp turret, unit, bool
    Shootp([Arg; 2]),
    /// configure building configuration
    Configure(Arg),
    /// color illuminator r g b
    Color([Arg; 3])
}

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

impl std::fmt::Display for Arg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", match self {
            Arg::Literal(t) => {
                match t {
                    Type::Num(f) => f.to_string(),
                    Type::Str(s) => s.clone()
                }
            },
            Arg::Variable(v) => v.clone()
        }).expect("Failed to write to Formatter");
        Ok(())
    }
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

impl std::str::FromStr for Operation {
    type Err = String;
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
            _ => return Err(format!("Unknown operation {}", s))
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
}

impl Type {

    /// Compares Type based on enum variant
    pub fn cmp(&self, other: &Self) -> bool {
        use std::mem::discriminant;
        discriminant(self) == discriminant(other)
    }
}

/// Inverts Comparison into it's opposite
impl std::ops::Neg for Comparison {
    type Output = Comparison;
    fn neg(self) -> Self::Output {
        use Comparison::*;
        match self {
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
            Always => panic!("Always cannot be negated!")
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn type_cmp() {
        use crate::compiler::instructions::{
            Arg::*, Type::*
        };
        assert!(Literal(Num(5.0)).cmp(&Literal(Num(10.0))));
        assert!(Literal(Str("Pain".into())).cmp(&Literal(Str("Suffering".into()))));
        assert!(Variable("Pain".into()).cmp(&Variable("Suffering".into())));
    }
}
