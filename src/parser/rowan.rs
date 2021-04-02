use super::ParseResult;
use crate::lexer::{Lexer, Token};
use num_traits::{FromPrimitive, ToPrimitive};
use rowan::{Checkpoint, GreenNodeBuilder, Language, SyntaxKind};
use std::iter::Peekable;

pub struct Parser<'a> {
    pub lexer: Peekable<Lexer<'a>>,
    builder: GreenNodeBuilder<'static>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> ParseResult {
        let tmp = Self {
            lexer: Lexer::new(input).peekable(),
            builder: GreenNodeBuilder::new(),
        };

        tmp.parse()
    }

    fn parse(mut self) -> ParseResult {
        self.start_node(Token::Root);

        self.type_expr();

        self.finish_node();

        ParseResult {
            root: self.builder.finish(),
        }
    }

    pub fn peek(&mut self) -> Option<Token> {
        self.lexer.peek().map(|(token, _)| *token)
    }

    pub fn push(&mut self) {
        let (token, raw) = self.lexer.next().unwrap();

        self.builder.token(token.into(), raw);

        if self.peek() == Some(Token::Whitespace) {
            self.push();
        }
    }

    pub fn start_node(&mut self, token: Token) {
        self.builder.start_node(token.into());
    }

    pub fn start_node_at(&mut self, checkpoint: Checkpoint, token: Token) {
        self.builder.start_node_at(checkpoint, token.into())
    }

    pub fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    pub fn checkpoint(&self) -> Checkpoint {
        self.builder.checkpoint()
    }
}

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum SetaLanguage {}

impl Language for SetaLanguage {
    type Kind = Token;

    fn kind_from_raw(raw: SyntaxKind) -> Self::Kind {
        Self::Kind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> SyntaxKind {
        SyntaxKind(kind.to_u16().unwrap())
    }
}

impl From<SyntaxKind> for Token {
    fn from(kind: SyntaxKind) -> Self {
        SetaLanguage::kind_from_raw(kind)
    }
}

impl From<Token> for SyntaxKind {
    fn from(token: Token) -> Self {
        SetaLanguage::kind_to_raw(token)
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;

    fn check(input: &str, result: &str) {
        assert_eq!(format!("{:?}", Parser::new(input)), result);
    }

    #[cfg(test)]
    mod types {
        use super::check;

        #[test]
        fn test_reference() {
            check(
                "ABC123\n",
                r#"Root@0..6
  Identifier@0..6 "ABC123""#,
            )
        }

        #[test]
        fn test_function() {
            check(
                "A->B->C",
                r#"Root@0..7
  BinaryOp@0..7
    Identifier@0..1 "A"
    ArrowOp@1..3 "->"
    BinaryOp@3..7
      Identifier@3..4 "B"
      ArrowOp@4..6 "->"
      Identifier@6..7 "C""#,
            )
        }

        #[test]
        fn test_intersection() {
            check(
                "A&B",
                r#"Root@0..3
  BinaryOp@0..3
    Identifier@0..1 "A"
    BitwiseAndOp@1..2 "&"
    Identifier@2..3 "B""#,
            )
        }

        #[test]
        fn test_union() {
            check(
                "A|B",
                r#"Root@0..3
  BinaryOp@0..3
    Identifier@0..1 "A"
    BitwiseOrOp@1..2 "|"
    Identifier@2..3 "B""#,
            )
        }

        #[test]
        fn test_order_of_operations() {
            check(
                "A&B|C&D",
                r#"Root@0..7
  BinaryOp@0..7
    BinaryOp@0..3
      Identifier@0..1 "A"
      BitwiseAndOp@1..2 "&"
      Identifier@2..3 "B"
    BitwiseOrOp@3..4 "|"
    BinaryOp@4..7
      Identifier@4..5 "C"
      BitwiseAndOp@5..6 "&"
      Identifier@6..7 "D""#,
            )
        }
    }
}
