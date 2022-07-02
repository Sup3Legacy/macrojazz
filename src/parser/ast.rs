use crate::common::location::Located;

/// The type for all identifiers
pub type Identifier = String;

/// A static 2-adic operator
#[derive(Debug)]
pub enum StaticBinOp {
    Plus,
    Minus,
    Div,
    Mult,
    Modulo,
    Or,
    And,
    Smaller,
    SmallerEq,
    Greater,
    GreaterEq,
}

/// A static 1-adic operator
#[derive(Debug)]
pub enum StaticMonOp {
    Minus,
    Not,
}

// A static immediate
#[derive(Debug)]
pub enum StaticImmediate {
    Int(usize),
}

#[derive(Debug)]
pub enum StaticExpression {
    Ident(Located<Identifier>),
    Immediate(Located<StaticImmediate>),
    MonOp {
        operand: Box<Located<StaticExpression>>,
        operator: Located<StaticMonOp>,
    },
    BinOp {
        lhs: Box<Located<StaticExpression>>,
        rhs: Box<Located<StaticExpression>>,
        operator: Located<StaticBinOp>,
    },
}
