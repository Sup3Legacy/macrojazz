mod common;
#[allow(dead_code)]
mod parser;

fn main() {
    let code = r"
node test<n: int>(a: [!n], b: [1]) -> (c: [n]) {
    c = a == 1 != 2;
}
";

    let res = parser::parse_single(code, common::location::SrcId::empty());

    println!("{:#?}", res);
}
