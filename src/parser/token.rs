use crate::common::location::Location;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    Bool(bool),
    Int(usize),

    Ident(String),

    Node,

    If,
    Else,
    End,

    Affect,
    ParL,
    ParR,

    Dot,
    DotDot,
    Comma,
    Colon,
    Semicolon,

    LT,
    Lte,
    Gt,
    Gte,
    Eq,

    Plus,
    Minus,
    Div,
    Mul,
}

impl Token {
    pub fn map_control(c: char) -> Self {
        match c {
            '(' => Token::ParL,
            ')' => Token::ParR,
            ',' => Token::Comma,
            ':' => Token::Colon,
            ';' => Token::Semicolon,
            _ => panic!("Unknown control character: {c}"),
        }
    }

    pub fn map_ident(s: String) -> Self {
        match s.as_str() {
            "node" => Token::Node,
            "if" => Token::If,
            "else" => Token::Else,
            "end" => Token::End,
            _ => Token::Ident(s),
        }
    }
}
