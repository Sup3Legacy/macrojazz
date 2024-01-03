use super::static_expr::*;
use super::type_tree::*;
use crate::parser::ast::*;
use crate::typing::*;
use crate::Located;
use std::collections::HashMap;
use z3::*;

#[derive(Debug)]
pub enum TypingError {
    IdentNotFound,
}

#[derive(Debug, Clone)]
pub enum ExpType {
    Bit,
    // INFO: used to cast an integer to any type of 1d vector
    // WARN: what semantics should be used to accomodate bitwidth?
    SizeLessVector,
    Vector(TT),
    // TODO: this is for multidimensional
    //Vector(Box<ExpType>, TT),
    Tuple(Vec<ExpType>),
}

pub type Identifier = String;
type ExpLocated<T> = Located<(), T>;
type ExpTypedLocated<T> = Located<ExpType, T>;

pub type Params = Vec<ExpLocated<Expression>>;

pub type StaticParams = Vec<StaticTypedLocated<StaticExpression>>;

#[derive(Debug)]
pub enum Index {
    Range {
        lhs: Option<StaticLocated<StaticExpression>>,
        rhs: Option<StaticLocated<StaticExpression>>,
        rhs_inclusive: bool,
    },
    Simple(StaticExpression),
}

#[derive(Debug)]
pub enum Literal {
    Int(u64),
}

impl Literal {
    pub fn from_early(el: EarlyLiteral) -> Self {
        match el {
            EarlyLiteral::Int(i) => Self::Int(i),
        }
    }
}

#[derive(Debug)]
pub enum MonOp {
    BitNot,
}

impl MonOp {
    pub fn from_early(em: EarlyMonOp) -> Self {
        match em {
            EarlyMonOp::BitNot => Self::BitNot,
        }
    }
}

#[derive(Debug)]
pub enum BinOp {
    Concat,
    BitOr,
    BitAnd,
    BitXOr,
}

impl BinOp {
    pub fn from_early(eb: EarlyBinOp) -> Self {
        match eb {
            EarlyBinOp::Concat => Self::Concat,
            EarlyBinOp::BitOr => Self::BitOr,
            EarlyBinOp::BitAnd => Self::BitAnd,
            EarlyBinOp::BitXOr => Self::BitXOr,
        }
    }
}

#[derive(Debug)]
pub enum Lhs {
    Ident(Identifier),
    Tuple(Vec<ExpLocated<Identifier>>),
    // TODO Add slice as a LHS?
}

#[derive(Debug)]
pub enum Expression {
    Ident(Identifier),
    Literal(Literal),
    MonOp {
        operand: Box<ExpTypedLocated<Expression>>,
        operator: ExpLocated<MonOp>,
    },
    BinOp {
        lhs: Box<ExpTypedLocated<Expression>>,
        rhs: Box<ExpTypedLocated<Expression>>,
        operator: ExpLocated<BinOp>,
    },
    FuncCall {
        func_name: ExpLocated<Identifier>,
        static_params: Option<StaticParams>,
        runtime_params: Params,
        builtin: ExpLocated<bool>,
    },
    Index {
        lhs: Box<ExpTypedLocated<Expression>>,
        index: Box<ExpLocated<Index>>,
    },
    Tuple(Vec<ExpTypedLocated<Expression>>),
    IfThenElse {
        condition: Box<StaticTypedLocated<StaticExpression>>,
        if_block: Box<ExpTypedLocated<Expression>>,
        else_block: Box<ExpTypedLocated<Expression>>,
    },
    Let {
        lhs: ExpLocated<Lhs>,
        rhs: Box<ExpTypedLocated<Expression>>,
        scope: Box<ExpTypedLocated<Expression>>,
    },
}

// TODO: fucntion to build z3 tree from StaticExpression

fn type_expression(
    exp: EarlyLocated<EarlyExpression>,
    z3ctx: &mut z3::Context,
    exp_ctx: &HashMap<String, ExpType>,
    static_ctx: &mut HashMap<String, StaticType>,
    // TODO: add function signature context
    // TODO: also need assumptions on meta-variables?
    // OR put them in the z3 context before calling this function, maybe
) -> Result<ExpTypedLocated<Expression>, ExpLocated<TypingError>> {
    let loc = exp.loc.clone();
    match exp.inner {
        EarlyExpression::Ident(id) => exp_ctx
            .get(&id)
            .ok_or(ExpLocated::__from_loc(
                TypingError::IdentNotFound,
                (),
                loc.clone(),
            ))
            .map(|v| ExpTypedLocated::__from_loc(Expression::Ident(id), (*v).clone(), loc)),
        EarlyExpression::Literal(i) => Ok(ExpTypedLocated::__from_loc(
            Expression::Literal(Literal::from_early(i)),
            ExpType::SizeLessVector,
            loc,
        )),
        EarlyExpression::MonOp { operand, operator } => {
            let typed_operand = type_expression(*operand, z3ctx, exp_ctx, static_ctx)?;
            let res_type = typed_operand.custom.clone();
            Ok(ExpTypedLocated::__from_loc(
                Expression::MonOp {
                    operand: Box::new(typed_operand),
                    operator: ExpLocated::__from_loc(
                        MonOp::from_early(operator.inner),
                        (),
                        operator.loc,
                    ),
                },
                res_type,
                loc,
            ))
        }
        EarlyExpression::BinOp { lhs, rhs, operator } => {
            let typed_lhs = type_expression(*lhs, z3ctx, exp_ctx, static_ctx)?;
            let typed_rhs = type_expression(*rhs, z3ctx, exp_ctx, static_ctx)?;

            match operator.inner {
                // INFO: Concat is a special case
                EarlyBinOp::Concat => {
                    let res_type = match (typed_lhs.custom.clone(), typed_rhs.custom.clone()) {
                        // Must not concatenate tuples
                        (_, ExpType::Tuple(_)) | (ExpType::Tuple(_), _) => todo!(),

                        // sizeless . sizelsss => sizeless
                        (ExpType::SizeLessVector, ExpType::SizeLessVector) => {
                            ExpType::SizeLessVector
                        }

                        // two bits
                        (ExpType::Bit, ExpType::Bit) => ExpType::Vector(TT::IntLiteral(2)),

                        // A bit and a vector
                        (ExpType::Bit, ExpType::Vector(lr)) => ExpType::Vector(TT::BinOp {
                            lhs: Box::new(lr.clone()),
                            rhs: Box::new(TT::IntLiteral(1)),
                            operator: TTBinOp::Plus,
                        }),
                        (ExpType::Vector(ll), ExpType::Bit) => ExpType::Vector(TT::BinOp {
                            lhs: Box::new(ll.clone()),
                            rhs: Box::new(TT::IntLiteral(1)),
                            operator: TTBinOp::Plus,
                        }),

                        // General case
                        (ExpType::Vector(ll), ExpType::Vector(lr)) => ExpType::Vector(TT::BinOp {
                            lhs: Box::new(ll.clone()),
                            rhs: Box::new(lr.clone()),
                            operator: TTBinOp::Plus,
                        }),

                        // TODO: Other wrong cases
                        (_, _) => todo!(),
                    };

                    Ok(ExpTypedLocated::__from_loc(
                        Expression::BinOp {
                            lhs: Box::new(typed_lhs),
                            rhs: Box::new(typed_rhs),
                            operator: ExpLocated::__from_loc(
                                BinOp::from_early(operator.inner),
                                (),
                                operator.loc,
                            ),
                        },
                        res_type,
                        loc,
                    ))
                }
                _ => todo!(),
            }
        }
        EarlyExpression::FuncCall {
            func_name,
            static_params,
            runtime_params,
            builtin,
        } => todo!(),
        EarlyExpression::Index { lhs, index } => todo!(),
        EarlyExpression::Tuple(_) => todo!(),
        EarlyExpression::IfThenElse {
            condition,
            if_block,
            else_block,
        } =>
        // TODO: z3 push
        // TODO: z3 pop
        {
            todo!()
        }
        // TODO: z3 push
        // TODO: z3 pop
        ,
        EarlyExpression::Let { lhs, rhs, scope } => todo!(),
    }
}
