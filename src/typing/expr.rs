use super::static_expr::*;
use super::type_tree::*;
use crate::parser::ast::*;
use crate::typing::*;
use crate::Located;
use std::collections::HashMap;
use z3::ast::Ast;

#[derive(Clone, Debug)]
pub enum TypingError {
    IdentNotFound,
    BinOpWrongSize,
    ConditionDoesntType,
    ConditionNotBool,
    WrongReturnType,
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
    IfT {
        condition: TT,
        if_branch: Box<ExpType>,
        else_branch: Box<ExpType>,
    },
}

impl ExpType {
    pub fn to_z3<'a>(self, ctx: &'a z3::Context, z3_env: &HashMap<String, TTz3<'a>>) -> TTz3<'a> {
        match self {
            ExpType::Bit => TTz3::bit(ctx),
            ExpType::SizeLessVector => TTz3::u64(ctx),
            ExpType::Vector(tt) => tt.to_z3(ctx, z3_env),
            ExpType::IfT {
                condition,
                if_branch,
                else_branch,
            } => {
                let condition_z3 = condition.to_z3(ctx, z3_env);
                let if_branch_z3 = (*if_branch).to_z3(ctx, z3_env);
                let else_branch_z3 = (*else_branch).to_z3(ctx, z3_env);

                match (condition_z3, if_branch_z3, else_branch_z3) {
                    (TTz3::Bool(bool_term), TTz3::Int(if_term), TTz3::Int(else_term)) => {
                        TTz3::Int(bool_term.ite::<z3::ast::Int>(&if_term, &else_term))
                    }
                    (TTz3::Int(_), _, _) => unreachable!(),
                    (_, _, _) => unreachable!(),
                }
            }
            ExpType::Tuple(_) => todo!(),
        }
    }
}

fn z3_compare_types<'a>(
    t1: ExpType,
    t2: ExpType,
    ctx: &z3::Context,
    solver: &z3::Solver,
    z3_env: &HashMap<String, TTz3<'a>>,
) -> bool {
    let z3_1 = t1.to_z3(ctx, z3_env);
    let z3_2 = t2.to_z3(ctx, z3_env);

    let equality = match (z3_1, z3_2) {
        (TTz3::Int(z1), TTz3::Int(z2)) => z1._eq(&z2),
        _ => todo!(),
    };

    let not_equality = equality.not();

    solver.assert(&not_equality);

    let result = solver.check();

    println!("Got {:?}", result);

    // TODO: is SAT, show a counter-example model!

    result == z3::SatResult::Unsat
}

pub type Identifier = String;
pub type ExpLocated<T> = Located<(), T>;
pub type ExpTypedLocated<T> = Located<ExpType, T>;

pub type Params = Vec<ExpLocated<Expression>>;

pub type StaticParams = Vec<StaticTypedLocated<StaticExpression>>;

#[derive(Clone, Debug)]
pub enum Index {
    Range {
        lhs: Option<StaticLocated<StaticExpression>>,
        rhs: Option<StaticLocated<StaticExpression>>,
        rhs_inclusive: bool,
    },
    Simple(StaticExpression),
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum Lhs {
    Ident(Identifier),
    Tuple(Vec<ExpLocated<Identifier>>),
    // TODO Add slice as a LHS?
}

#[derive(Clone, Debug)]
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

fn type_expression<'a>(
    exp: EarlyLocated<EarlyExpression>,
    z3_ctx: &z3::Context,
    z3_env: &HashMap<String, TTz3<'a>>,
    solver: &mut z3::Solver,
    exp_ctx: &HashMap<String, ExpType>,
    static_ctx: &HashMap<String, StaticType>,
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
            let typed_operand =
                type_expression(*operand, z3_ctx, z3_env, solver, exp_ctx, static_ctx)?;
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
            let typed_lhs = type_expression(*lhs, z3_ctx, z3_env, solver, exp_ctx, static_ctx)?;
            let typed_rhs = type_expression(*rhs, z3_ctx, z3_env, solver, exp_ctx, static_ctx)?;

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
                // Other binary operators
                _ => {
                    let (t1, t2) = (typed_lhs.custom.clone(), typed_rhs.custom.clone());
                    let does_type = z3_compare_types(t1.clone(), t2, z3_ctx, solver, z3_env);
                    if does_type {
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
                            t1,
                            loc,
                        ))
                    } else {
                        Err(ExpLocated::__from_loc(TypingError::BinOpWrongSize, (), loc))
                    }
                }
            }
        }
        EarlyExpression::FuncCall {
            func_name: _,
            static_params: _,
            runtime_params: _,
            builtin: _,
        } => todo!(),
        EarlyExpression::Index { lhs: _, index: _ } => todo!(),
        EarlyExpression::Tuple(_) => todo!(),
        EarlyExpression::IfThenElse {
            condition,
            if_block,
            else_block,
        } => {
            let loc_ = condition.loc.clone();
            let loc__ = condition.loc.clone();
            let typed_condition = type_static(*condition, static_ctx)
                .map_err(|_| ExpLocated::__from_loc(TypingError::ConditionDoesntType, (), loc_))?;

            if typed_condition.custom != StaticType::Bool {
                Err(ExpLocated::__from_loc(
                    TypingError::ConditionNotBool,
                    (),
                    loc__,
                ))
            } else {
                // push and pop assertions to the z3 solver to further refine the model
                let z3_condition_raw = typed_condition.clone();
                let condition_tt = typed_condition.clone().to_tt();
                let z3_condition = match typed_condition.to_tt().to_z3(z3_ctx, z3_env) {
                    TTz3::Bool(b) => b,
                    _ => todo!(),
                };

                solver.push();
                // WARN
                solver.assert(&z3_condition);
                println!("Entering if branch");

                let typed_if =
                    type_expression(*if_block, z3_ctx, z3_env, solver, exp_ctx, static_ctx)?;
                let if_type = typed_if.clone().custom;

                solver.pop(1);
                solver.push();

                solver.assert(&z3_condition.not());
                println!("Entering else branch");

                let typed_else =
                    type_expression(*else_block, z3_ctx, z3_env, solver, exp_ctx, static_ctx)?;
                let else_type = typed_else.clone().custom;
                solver.pop(1);

                Ok(ExpTypedLocated::__from_loc(
                    Expression::IfThenElse {
                        condition: Box::new(z3_condition_raw),
                        if_block: Box::new(typed_if),
                        else_block: Box::new(typed_else),
                    },
                    ExpType::IfT {
                        condition: condition_tt,
                        if_branch: Box::new(if_type),
                        else_branch: Box::new(else_type),
                    },
                    loc,
                ))
            }
        }
        EarlyExpression::Let {
            lhs: _,
            rhs: _,
            scope: _,
        } => todo!(),
    }
}

impl EarlyLocated<EarlyType> {
    pub fn to_exptype(self, env: &HashMap<String, StaticType>) -> ExpType {
        match self.inner {
            EarlyType::Tuple(type_vec) => ExpType::Tuple(
                type_vec
                    .into_iter()
                    .map(|v| v.to_exptype(env))
                    .collect::<Vec<_>>(),
            ),
            EarlyType::Base(static_exp) => {
                ExpType::Vector(type_static(static_exp, env).expect("truc").to_tt())
            }
            EarlyType::Unit => ExpType::Bit,
        }
    }
}

// TODO: should take as argument an environment with all node names and signatures
pub fn type_check_node(
    located_node: EarlyLocated<EarlyNode>,
) -> Result<ExpTypedLocated<Expression>, ExpLocated<TypingError>> {
    let z3_cfg = z3::Config::new();
    let z3_ctx = z3::Context::new(&z3_cfg);
    let mut z3_solver = z3::Solver::new(&z3_ctx);

    let mut z3_env = HashMap::new();
    let mut exp_ctx = HashMap::new();
    let mut static_ctx = HashMap::new();

    let node = located_node.inner;

    if let Some(static_params) = node.static_args {
        // Add all idents to the env and nothing else
        for located_sparam in static_params.inner.iter() {
            let sparam = &located_sparam.inner;
            let sparam_type = sparam
                .typ
                .as_ref()
                .expect("Want a type for static parameter.")
                .clone();
            let base_type = &sparam_type.inner.base.expect("Want a base type");

            println!(
                "Insert type for static parameter {}",
                sparam.name.inner.clone()
            );
            static_ctx.insert(
                sparam.name.inner.clone(),
                StaticType::from_early(base_type.clone()),
            );
        }

        // Handle type refinements
        for located_sparam in static_params.inner {
            let sparam = located_sparam.inner;
            let sparam_type = sparam.typ.expect("Want a type for static parameter.");
            let base_type = sparam_type.inner.base.expect("Want a base type");
            let refinement_opt = sparam_type.inner.refinement;

            let name = sparam.name.inner.clone();
            println!("Insert z3 constant for static parameter {}", name.as_str());
            match base_type.inner {
                EarlyStaticBaseType::Int => {
                    let lhs = z3::ast::Int::new_const(&z3_ctx, sparam.name.inner.clone());
                    z3_env.insert(sparam.name.inner, TTz3::Int(lhs));
                }
                EarlyStaticBaseType::Bool => {
                    let lhs = z3::ast::Bool::new_const(&z3_ctx, sparam.name.inner.clone());
                    z3_env.insert(sparam.name.inner, TTz3::Bool(lhs));
                }
            };

            if let Some(refinement) = refinement_opt {
                let typed_refinement = type_static(refinement, &static_ctx)
                    .expect("Could not type static param refinement expression.")
                    .to_tt()
                    .to_z3(&z3_ctx, &z3_env);

                println!(
                    "Insert z3 refinement constraint for static parameter {}",
                    name.as_str()
                );
                match typed_refinement {
                    TTz3::Int(_) => panic!("Refinement expression must have Bool type"),
                    TTz3::Bool(b) => {
                        z3_solver.assert(&b);
                    }
                }
            }
        }
    }

    for located_dparam in node.runtime_args.inner {
        let dparam = located_dparam.inner;
        let dparam_type = dparam
            .typ
            .expect("Must provide concrete type")
            .to_exptype(&static_ctx);
        exp_ctx.insert(dparam.name.inner.clone(), dparam_type);
    }

    let block_typexp = type_expression(
        node.block,
        &z3_ctx,
        &z3_env,
        &mut z3_solver,
        &exp_ctx,
        &static_ctx,
    )?;

    let block_type = block_typexp.custom.clone();
    let node_return_type = node.runtime_outs.to_exptype(&static_ctx);

    let compare = z3_compare_types(block_type, node_return_type, &z3_ctx, &z3_solver, &z3_env);

    if !compare {
        println!("Bad return type");
        return Err(ExpLocated::empty_from_range(
            TypingError::WrongReturnType,
            (),
            0..0,
        ));
    }

    Ok(block_typexp)
}
