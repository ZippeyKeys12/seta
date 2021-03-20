use crate::lexer::{Lexer, Token};
use crate::parser::ParseResult;
use num_traits::{FromPrimitive, ToPrimitive};
use rowan::{Checkpoint, GreenNodeBuilder, Language, SyntaxKind};
use std::iter::Peekable;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
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

        self.builder.token(token.into(), raw)
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
