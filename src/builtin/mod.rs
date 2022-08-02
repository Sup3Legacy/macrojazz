/// A builtin operator declaration.
/// This makes it possible to easily customize builtins.
#[derive(Debug)]
pub struct Builtin {
    name: &'static str,
}
