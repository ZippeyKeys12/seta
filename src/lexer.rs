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

    fn test_complete(input: &str, token: Token) {
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next(), Some((token, input)));
    }

    // Keywords
    #[test]
    fn test_type() {
        test_complete("type", Token::Type)
    }

    #[test]
    fn test_fn() {
        test_complete("fn", Token::Fn)
    }

    // Symbols

    #[test]
    fn test_colon() {
        test_complete(":", Token::Colon)
    }

    #[test]
    fn test_line_ending() {
        test_complete("\n", Token::LineEnding);
        test_complete(";", Token::LineEnding)
    }

    // Pairs

    #[test]
    fn test_parens() {
        test_complete("(", Token::LParen);
        test_complete(")", Token::RParen);
    }

    #[test]
    fn test_brace() {
        test_complete("{", Token::LBrace);
        test_complete("}", Token::RBrace);
    }

    #[test]
    fn test_bracket() {
        test_complete("[", Token::LBracket);
        test_complete("]", Token::RBracket);
    }

    #[test]
    fn test_chevron() {
        test_complete("<", Token::LChevron);
        test_complete(">", Token::RChevron);
    }

    // Operators
    #[test]
    fn test_dot() {
        test_complete(".", Token::DotOp)
    }

    #[test]
    fn test_assign() {
        test_complete("=", Token::AssignOp)
    }

    #[test]
    fn test_arrow() {
        test_complete("->", Token::ArrowOp)
    }

    // Boolean

    #[test]
    fn test_not() {
        test_complete("not", Token::NotOp)
    }

    #[test]
    fn test_lor() {
        test_complete("or", Token::LogicalOrOp)
    }

    #[test]
    fn test_land() {
        test_complete("and", Token::LogicalAndOp)
    }

    #[test]
    fn test_lxor() {
        test_complete("xor", Token::LogicalXorOp)
    }

    #[test]
    fn test_equals() {
        test_complete("==", Token::EqOp)
    }

    #[test]
    fn test_nequals() {
        test_complete("!=", Token::NotEqOp)
    }

    // Bitwise

    #[test]
    fn test_neg() {
        test_complete("~", Token::BitwiseNegOp)
    }

    #[test]
    fn test_bor() {
        test_complete("|", Token::BitwiseOrOp)
    }

    #[test]
    fn test_band() {
        test_complete("&", Token::BitwiseAndOp)
    }

    #[test]
    fn test_bxor() {
        test_complete("^", Token::BitwiseXorOp)
    }

    // Math

    #[test]
    fn test_add() {
        test_complete("+", Token::AddOp)
    }

    #[test]
    fn test_sub() {
        test_complete("-", Token::SubOp)
    }

    #[test]
    fn test_mul() {
        test_complete("*", Token::MulOp)
    }

    #[test]
    fn test_div() {
        test_complete("/", Token::DivOp)
    }

    //

    #[test]
    fn test_identifier() {
        test_complete("asdadsdas02dsa", Token::Identifier);
    }

    #[test]
    fn test_spaces() {
        test_complete("  	", Token::Whitespace);
    }
}
