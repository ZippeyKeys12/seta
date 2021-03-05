use num_traits::{FromPrimitive, ToPrimitive};
use rowan::{GreenNode, GreenNodeBuilder, Language, SyntaxKind, SyntaxNode};

use std::{fmt, iter::Peekable};

use crate::lexer::{Lexer, Token};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    builder: GreenNodeBuilder<'static>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            lexer: Lexer::new(input).peekable(),
            builder: GreenNodeBuilder::new(),
        }
    }

    pub fn parse(mut self) -> ParseResult {
        self.start_node(Token::Root);
        self.finish_node();

        ParseResult {
            root: self.builder.finish(),
        }
    }

    fn peek(&mut self) -> Option<Token> {
        self.lexer.peek().map(|(token, _)| *token)
    }

    fn push(&mut self) {
        let (token, raw) = self.lexer.next().unwrap();

        self.builder.token(token.into(), raw)
    }

    fn start_node(&mut self, token: Token) {
        self.builder.start_node(token.into());
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }
}

pub struct ParseResult {
    root: GreenNode,
}

impl fmt::Debug for ParseResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tmp = format!(
            "{:#?}",
            SyntaxNode::<SetaLanguage>::new_root(self.root.clone())
        );
        write!(f, "{}", &tmp[0..tmp.len() - 1]);

        Ok(())
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
