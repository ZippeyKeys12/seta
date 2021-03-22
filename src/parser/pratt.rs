use crate::{
    lexer::{Lexer, Token},
    parser::{ParseResult, Parser},
};
use rowan::GreenNode;
use std::iter::Peekable;

const MAX_PRECEDENCE: u8 = u8::max_value();

pub trait PrattParser {
    type Op: PrattOp;

    fn get_prefix(&self, lexer: &mut Peekable<Lexer>) -> Option<Self::Op>;
    fn get_mixfix(&self, lexer: &mut Peekable<Lexer>) -> Option<Self::Op>;

    fn is_prefix(&self, lexer: &mut Peekable<Lexer>) -> bool {
        self.get_prefix(lexer).is_some()
    }
    fn is_mixfix(&self, lexer: &mut Peekable<Lexer>) -> bool {
        self.get_mixfix(lexer).is_some()
    }
    fn get_precedence(&self, lexer: &mut Peekable<Lexer>) -> u8 {
        let op = self.get_mixfix(lexer);

        if let Some(op) = op {
            op.precedence()
        } else {
            u8::min_value()
        }
    }
}

pub trait PrattOp {
    fn precedence(&self) -> u8;
    fn parse(&self, parser: &mut Parser, pratt: &dyn PrattParser<Op = Self>);
}

pub fn pratt_parse<Op>(parser: &mut Parser, pratt: &dyn PrattParser<Op = Op>)
where
    Op: PrattOp,
{
    pratt_parse_prec(parser, pratt, 1);
}

pub fn pratt_parse_prec<Op>(parser: &mut Parser, pratt: &dyn PrattParser<Op = Op>, precedence: u8)
where
    Op: PrattOp,
{
    let checkpoint = parser.checkpoint();

    let op = pratt.get_prefix(&mut parser.lexer);

    if let Some(_) = op {
        parser.push();

        while precedence < pratt.get_precedence(&mut parser.lexer) {
            let mop = pratt.get_mixfix(&mut parser.lexer).unwrap();
            parser.push();

            parser.start_node_at(checkpoint, Token::BinaryOp);
            mop.parse(parser, pratt);
            parser.finish_node();

            if !pratt.is_mixfix(&mut parser.lexer) {
                break;
            }
        }
    }
}
