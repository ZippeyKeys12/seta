use crate::{
    lexer::{Lexer, Token},
    parser::{ParseResult, Parser},
};
use rowan::GreenNode;
use std::iter::Peekable;

const MAX_PRECEDENCE: u8 = u8::max_value();

trait PrattParser {
    type Op: PrattOp;

    fn get_prefix(&self, token: Token) -> Option<Self::Op>;
    fn get_mixfix(&self, token: Token) -> Option<Self::Op>;

    fn get_precedence(&self, token: Token) -> u8 {
        let op = self.get_mixfix(token);

        if let Some(op) = op {
            op.precedence()
        } else {
            u8::min_value()
        }
    }
    fn get_associativity(&self, token: Token) -> PrattAssoc {
        let op = self.get_mixfix(token).unwrap();
        op.associativity()
    }
}

trait PrattOp {
    fn precedence(&self) -> u8;
    fn associativity(&self) -> PrattAssoc;
}

enum PrattAssoc {
    Right,
    Left,
}

impl<'a> Parser<'a> {
    fn pratt_parse<Op>(&mut self, pratt: &dyn PrattParser<Op = Op>)
    where
        Op: PrattOp,
    {
        self.pratt_parse_prec(pratt, MAX_PRECEDENCE);
    }

    fn pratt_parse_prec<Op>(&mut self, pratt: &dyn PrattParser<Op = Op>, precedence: u8)
    where
        Op: PrattOp,
    {
        let checkpoint = self.checkpoint();

        let op = pratt.get_prefix(self.peek().unwrap());

        if let Some(_) = op {
            self.push();

            let mut mop = self.peek().unwrap();
            while precedence < pratt.get_precedence(mop) {
                self.push();
                self.start_node_at(checkpoint, mop);
                match pratt.get_associativity(mop) {
                    PrattAssoc::Right => self.pratt_parse_prec(pratt, precedence - 1),
                    PrattAssoc::Left => self.pratt_parse_prec(pratt, precedence),
                }
                self.finish_node();

                mop = self.peek().unwrap();
            }
        }
    }
}
