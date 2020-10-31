use super::general::{identifier, line_terminator, Definition, TypeDecl};

pub mod security;
pub mod shapes;

use security::{sec_type_expr, SecurityType};
use shapes::{shape_expr, ShapeType};

use std::{collections::HashMap, fmt, iter::FromIterator, str::FromStr};

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{all_consuming, complete},
    combinator::{map, opt},
    error::ErrorKind,
    multi::separated_list,
    sequence::{delimited, separated_pair, terminated, tuple},
    IResult,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Type {
    pub shape: Box<ShapeType>,
    pub security: Box<SecurityType>,
}

impl<'a> FromStr for Type {
    type Err = nom::Err<ErrorKind>;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        match all_consuming(complete(type_expr))(input) {
            Ok(ok) => Ok(*ok.1),
            Err(nom::Err::Error(err)) => Err(nom::Err::Error(err.1)),
            Err(nom::Err::Failure(err)) => Err(nom::Err::Failure(err.1)),
            _ => Err(nom::Err::Error(ErrorKind::Eof)),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(f, "{}{}", self.shape, self.security)
    }
}

pub fn type_expr(input: &str) -> IResult<&str, Box<Type>> {
    map(
        tuple((ws!(shape_expr), opt(ws!(sec_type_expr)))),
        |(shpe, sec)| {
            Box::new(Type {
                shape: shpe,
                security: match sec {
                    Some(s) => s,
                    None => Box::new(SecurityType::Top),
                },
            })
        },
    )(input)
}

pub struct TypeSpec(String, Type);

pub fn type_spec(input: &str) -> IResult<&str, (&str, Box<Type>)> {
    separated_pair(ws!(identifier), ws!(tag(":")), ws!(type_expr))(input)
}

pub fn type_spec_list(input: &str) -> IResult<&str, HashMap<&str, Box<Type>>> {
    let (input, list) = ws!(separated_list(ws!(tag(",")), ws!(type_spec)))(input)?;

    Ok((input, HashMap::from_iter(list)))
}

pub fn type_decl(input: &str) -> IResult<&str, Definition> {
    let (input, name) = delimited(ws!(tag("type")), ws!(identifier), ws!(tag("=")))(input)?;
    let (input, typ) = terminated(ws!(type_expr), line_terminator)(input)?;

    Ok((
        input,
        Definition::Type(TypeDecl {
            name: name.to_string(),
            value: *typ,
        }),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::collections::HashSet;

    #[test]
    fn test_type_expr() {
        let test_values = vec![
            (
                "A & B {_}",
                Type {
                    shape: Box::new(ShapeType::Intersection(
                        Box::new(ShapeType::Reference("A".to_string())),
                        Box::new(ShapeType::Reference("B".to_string())),
                    )),

                    security: Box::new(SecurityType::Top),
                },
            ),
            (
                "A -> D {R, S, T}",
                Type {
                    shape: Box::new(ShapeType::Function(
                        Box::new(ShapeType::Reference("A".to_string())),
                        Box::new(ShapeType::Reference("D".to_string())),
                    )),

                    security: Box::new(SecurityType::Literal({
                        let mut tmp = HashSet::new();
                        tmp.extend(vec!["R".to_string(), "S".to_string(), "T".to_string()]);
                        tmp
                    })),
                },
            ),
        ];

        for (test_val, ans) in test_values {
            let (i, t) = type_expr(test_val).unwrap();
            assert_eq!(*t, ans);
            assert_eq!(i, "");
        }
    }

    #[test]
    fn test_type_decl() {
        let test_values = vec![
            (
                "type A = Int | Float;",
                TypeDecl {
                    name: "A".to_string(),
                    value: Type {
                        shape: Box::new(ShapeType::Union(
                            Box::new(ShapeType::Reference("Int".to_string())),
                            Box::new(ShapeType::Reference("Float".to_string())),
                        )),

                        security: Box::new(SecurityType::Top),
                    },
                },
            ),
            (
                "type B=(Int, Float);",
                TypeDecl {
                    name: "B".to_string(),
                    value: Type {
                        shape: Box::new(ShapeType::TupleLiteral(vec![
                            ShapeType::Reference("Int".to_string()),
                            ShapeType::Reference("Float".to_string()),
                        ])),

                        security: Box::new(SecurityType::Top),
                    },
                },
            ),
            (
                "type C=(a:Int, b:Float) ;",
                TypeDecl {
                    name: "C".to_string(),
                    value: Type {
                        shape: Box::new(ShapeType::RecordLiteral({
                            let mut tmp = HashMap::new();
                            tmp.insert("a".to_string(), ShapeType::Reference("Int".to_string()));
                            tmp.insert("b".to_string(), ShapeType::Reference("Float".to_string()));
                            tmp
                        })),

                        security: Box::new(SecurityType::Top),
                    },
                },
            ),
        ];

        for (test_val, ans) in test_values {
            let (i, t) = type_decl(test_val).unwrap();

            if let Definition::Type(typ) = t {
                assert_eq!(typ, ans);
                assert_eq!(i, "");
            }
        }
    }
}
