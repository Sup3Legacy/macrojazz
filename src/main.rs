mod common;
#[allow(dead_code)]
mod parser;

fn main() {
    let code = r"
node test<n: int>(a: [!n], b: [1]) -> (c: [n]) {
    c = f<a,if a==b {a} else {b}>(1, 2) ^ 2 | a[..=n - 2] . if n == n { b } else { c };
}
";

    let res = parser::parse_single(code, common::location::SrcId::empty());

    println!("{:#?}", res);
}
