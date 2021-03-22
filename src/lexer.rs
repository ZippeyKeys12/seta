use logos::{self, Logos};
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Logos, Clone, Copy, Debug, FromPrimitive, PartialEq, ToPrimitive)]
pub enum Token {
    // Keywords
    #[token("type")]
    Type,
    #[token("fn")]
    Fn,

    // Symbols
    #[token(":")]
    Colon,
    #[regex(";|\n")]
    LineEnding,

    // Pairs
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("<")]
    LChevron,
    #[token(">")]
    RChevron,

    // Operators
    #[token(".")]
    DotOp,
    #[token("=")]
    AssignOp,
    #[token("->")]
    ArrowOp,

    // Boolean
    #[token("not")]
    NotOp,
    #[token("or")]
    LogicalOrOp,
    #[token("and")]
    LogicalAndOp,
    #[token("xor")]
    LogicalXorOp,
    #[token("==")]
    EqOp,
    #[token("!=")]
    NotEqOp,

    // Bitwise
    #[token("~")]
    BitwiseNegOp,
    #[token("|")]
    BitwiseOrOp,
    #[token("&")]
    BitwiseAndOp,
    #[token("^")]
    BitwiseXorOp,

    // Math
    #[token("+")]
    AddOp,
    #[token("-")]
    SubOp,
    #[token("*")]
    MulOp,
    #[token("/")]
    DivOp,

    //
    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    Identifier,

    #[regex("[\t ]+")]
    Whitespace,

    #[error]
    Error,

    // For rowan
    Root,
    BinaryOp,
}

pub struct Lexer<'a> {
    lex: logos::Lexer<'a, Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lex: Token::lexer(input),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        let token = self.lex.next()?;
        let raw = self.lex.slice();

        Some((token, raw))
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, Token};

    fn check(input: &str, token: Token) {
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some((token, input)));
    }

    // Keywords
    #[test]
    fn test_type() {
        check("type", Token::Type)
    }

    #[test]
    fn test_fn() {
        check("fn", Token::Fn)
    }

    // Symbols

    #[test]
    fn test_colon() {
        check(":", Token::Colon)
    }

    #[test]
    fn test_line_ending() {
        check("\n", Token::LineEnding);
        check(";", Token::LineEnding)
    }

    // Pairs

    #[test]
    fn test_parens() {
        check("(", Token::LParen);
        check(")", Token::RParen);
    }

    #[test]
    fn test_brace() {
        check("{", Token::LBrace);
        check("}", Token::RBrace);
    }

    #[test]
    fn test_bracket() {
        check("[", Token::LBracket);
        check("]", Token::RBracket);
    }

    #[test]
    fn test_chevron() {
        check("<", Token::LChevron);
        check(">", Token::RChevron);
    }

    // Operators
    #[test]
    fn test_dot() {
        check(".", Token::DotOp)
    }

    #[test]
    fn test_assign() {
        check("=", Token::AssignOp)
    }

    #[test]
    fn test_arrow() {
        check("->", Token::ArrowOp)
    }

    // Boolean

    #[test]
    fn test_not() {
        check("not", Token::NotOp)
    }

    #[test]
    fn test_lor() {
        check("or", Token::LogicalOrOp)
    }

    #[test]
    fn test_land() {
        check("and", Token::LogicalAndOp)
    }

    #[test]
    fn test_lxor() {
        check("xor", Token::LogicalXorOp)
    }

    #[test]
    fn test_equals() {
        check("==", Token::EqOp)
    }

    #[test]
    fn test_nequals() {
        check("!=", Token::NotEqOp)
    }

    // Bitwise

    #[test]
    fn test_neg() {
        check("~", Token::BitwiseNegOp)
    }

    #[test]
    fn test_bor() {
        check("|", Token::BitwiseOrOp)
    }

    #[test]
    fn test_band() {
        check("&", Token::BitwiseAndOp)
    }

    #[test]
    fn test_bxor() {
        check("^", Token::BitwiseXorOp)
    }

    // Math

    #[test]
    fn test_add() {
        check("+", Token::AddOp)
    }

    #[test]
    fn test_sub() {
        check("-", Token::SubOp)
    }

    #[test]
    fn test_mul() {
        check("*", Token::MulOp)
    }

    #[test]
    fn test_div() {
        check("/", Token::DivOp)
    }

    //

    #[test]
    fn test_identifier() {
        check("asdadsdas02dsa", Token::Identifier);
    }

    #[test]
    fn test_spaces() {
        check("  	", Token::Whitespace);
    }
}
