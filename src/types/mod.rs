mod security;
mod shapes;

use std::fmt;

use nom::{combinator::map, sequence::tuple, IResult};

pub struct Type {
    shape: Box<shapes::ShapeType>,
    security: Box<security::SecurityType>,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}{}", self.shape, self.security)
    }
}

pub fn type_expr(input: &str) -> IResult<&str, Box<Type>> {
    map(
        tuple((shapes::shape_expr, security::sec_type_expr)),
        |(shpe, sec)| {
            Box::new(Type {
                shape: shpe,
                security: sec,
            })
        },
    )(input)
}
