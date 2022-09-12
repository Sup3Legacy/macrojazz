#[derive(Debug, Clone, Copy, PartialEq, Eq, Ord, PartialOrd)]
pub enum SMTResult {
    Unknown,
    Unsat,
    Sat,
}

/// A trait for using wrappers around arbitrary SMT solvers.
trait SMTProvider {
    /// Set up the SMT solver
    fn setup(&mut self) -> Result<(), Box<dyn std::error::Error>>;

    /// Initialize the model with the generic definition and their (refined) type
    fn init(&mut self);

    /// Push a new term
    /// Typically used when entering a conditional branch
    fn push(&mut self);

    /// Pop the last term
    /// Typically used when exiting a conditional branch
    fn pop(&mut self);

    /// Spawn the SMT job and return the result
    fn solve(&mut self) -> SMTResult;
}
