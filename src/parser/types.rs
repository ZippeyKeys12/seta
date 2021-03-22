use super::{
    pratt::{pratt_parse, pratt_parse_prec, PrattOp, PrattParser},
    Parser,
};
use crate::lexer::{Lexer, Token};
use std::iter::Peekable;

#[derive(Debug, Default, PartialEq)]
pub struct TypeExpressionParser();

#[derive(Debug, PartialEq)]
pub enum TypeOperators {
    Intersection,
    Union,
    Function,
    Reference,
}

impl PrattParser for TypeExpressionParser {
    type Op = TypeOperators;

    fn get_prefix(&self, lexer: &mut Peekable<Lexer>) -> Option<TypeOperators> {
        Some(match lexer.peek().unwrap().0 {
            Token::Identifier => TypeOperators::Reference,

            _ => return None,
        })
    }

    fn get_mixfix(&self, lexer: &mut Peekable<Lexer>) -> Option<TypeOperators> {
        if let Some((token, _)) = lexer.peek() {
            Some(match token {
                Token::BitwiseAndOp => TypeOperators::Intersection,
                Token::BitwiseOrOp => TypeOperators::Union,
                Token::ArrowOp => TypeOperators::Function,

                _ => return None,
            })
        } else {
            None
        }
    }
}

impl PrattOp for TypeOperators {
    fn precedence(&self) -> u8 {
        match self {
            Self::Intersection => 100,
            Self::Union => 90,
            Self::Function => 80,

            _ => u8::min_value(),
        }
    }

    fn parse(&self, parser: &mut Parser, pratt: &dyn PrattParser<Op = Self>) {
        pratt_parse_prec(parser, pratt, self.precedence() - 1)
    }
}

impl<'a> Parser<'a> {
    pub fn type_expr(&mut self) {
        pratt_parse(self, &TypeExpressionParser::default())
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    fn check(
        input: &str,
        func: for<'a> fn(&'a mut Peekable<Lexer>) -> Option<TypeOperators>,
        result: Option<TypeOperators>,
    ) {
        let mut lexer = Lexer::new(input).peekable();
        assert_eq!(func(&mut lexer), result);
    }

    fn check_prefix(input: &str, result: Option<TypeOperators>) {
        check(
            input,
            |x| TypeExpressionParser::default().get_prefix(x),
            result,
        );
    }

    fn check_mixfix(input: &str, result: Option<TypeOperators>) {
        check(
            input,
            |x| TypeExpressionParser::default().get_mixfix(x),
            result,
        );
    }

    #[test]
    fn test_identifier() {
        check_prefix("ABC", Some(TypeOperators::Reference));
    }

    #[test]
    fn test_function() {
        check_mixfix("->", Some(TypeOperators::Function));
    }

    #[test]
    fn test_union() {
        check_mixfix("|", Some(TypeOperators::Union));
    }

    #[test]
    fn test_intersection() {
        check_mixfix("&", Some(TypeOperators::Intersection));
    }
}
