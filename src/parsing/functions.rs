use super::{
    docs::docstring,
    expression::{val_expr, Expression},
    general::{identifier, line_terminator, Definition, FunctionDecl},
    types::{
        security::SecurityType, shapes::ShapeType, type_expr, type_spec, type_spec_list, Type,
    },
};

use std::collections::HashMap;

extern crate toml;

extern crate nom;
use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::{delimited, preceded, tuple},
    IResult,
};

pub fn function_decl(input: &str) -> IResult<&str, Definition> {
    let (input, (doc, (name, arguments, ret), _, body, _)) = tuple((
        opt(ws!(docstring)),
        ws!(function_sig),
        ws!(tag("=")),
        ws!(val_expr),
        opt(ws!(line_terminator)),
    ))(input)?;

    let mut map = HashMap::<String, Box<Type>>::new();

    for (k, v) in arguments {
        map.insert(k.to_string(), v);
    }

    Ok((
        input,
        Definition::Function(FunctionDecl {
            doc,
            name: name.to_string(),
            parameters: map,
            ret: (ret.0.to_string(), ret.1),
            body,
        }),
    ))
}

type FunctionResult<'a> =
    IResult<&'a str, (&'a str, HashMap<&'a str, Box<Type>>, (&'a str, Box<Type>))>;

pub fn function_sig(input: &str) -> FunctionResult {
    tuple((
        ws!(preceded(ws!(tag("fn")), ws!(identifier))),
        // Parameters
        ws!(delimited(ws!(tag("(")), type_spec_list, ws!(tag(")")))),
        ws!(alt((
            preceded(
                tag("->"),
                // Gets name for return value, or defaults to 'ret'
                ws!(alt((type_spec, map(type_expr, |i| ("ret", i))))),
            ),
            |i| Ok((
                i,
                (
                    "ret",
                    Box::new(Type {
                        shape: Box::new("Unit".parse::<ShapeType>().unwrap()),
                        security: Box::new(SecurityType::Top)
                    })
                )
            ))
        ))),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    use super::super::types::{security::SecurityType, shapes::ShapeType};

    #[test]
    fn test_function_sig() {
        let test_values = vec![
            (
                "fn a() -> A",
                (
                    "a",
                    HashMap::<&str, Box<Type>>::new(),
                    (
                        "ret",
                        Box::new(Type {
                            shape: Box::new(ShapeType::Reference("A".to_string())),
                            security: Box::new(SecurityType::Top),
                        }),
                    ),
                ),
            ),
            (
                "fn b(a: A) -> B",
                (
                    "b",
                    {
                        let mut map = HashMap::<&str, Box<Type>>::new();
                        map.insert(
                            "a",
                            Box::new(Type {
                                shape: Box::new(ShapeType::Reference("A".to_string())),
                                security: Box::new(SecurityType::Top),
                            }),
                        );

                        map
                    },
                    (
                        "ret",
                        Box::new(Type {
                            shape: Box::new(ShapeType::Reference("B".to_string())),
                            security: Box::new(SecurityType::Top),
                        }),
                    ),
                ),
            ),
            (
                "fn bc(a: A, c: C {}) -> rt: B",
                (
                    "bc",
                    {
                        let mut map = HashMap::<&str, Box<Type>>::new();
                        map.insert(
                            "a",
                            Box::new(Type {
                                shape: Box::new(ShapeType::Reference("A".to_string())),
                                security: Box::new(SecurityType::Top),
                            }),
                        );
                        map.insert(
                            "c",
                            Box::new(Type {
                                shape: Box::new(ShapeType::Reference("C".to_string())),
                                security: Box::new(SecurityType::Bottom),
                            }),
                        );

                        map
                    },
                    (
                        "rt",
                        Box::new(Type {
                            shape: Box::new(ShapeType::Reference("B".to_string())),
                            security: Box::new(SecurityType::Top),
                        }),
                    ),
                ),
            ),
        ];

        for (test_val, ans) in test_values {
            let (i, f) = function_sig(test_val).unwrap();

            assert_eq!(f, ans);
            assert_eq!(i, "");
        }
    }

    #[test]
    fn test_function_decl() {
        let test_values = vec![
            (
                "fn a() -> B = 1",
                Definition::Function(FunctionDecl {
                    doc: None,
                    name: "a".to_string(),
                    parameters: HashMap::new(),
                    ret: ("ret".to_string(), Box::new("B".parse::<Type>().unwrap())),
                    body: Box::new(Expression::IntLiteral(1)),
                }),
            ),
            (
                "fn abc(a: Int, b: Int {}) = 1",
                Definition::Function(FunctionDecl {
                    doc: None,
                    name: "abc".to_string(),
                    parameters: {
                        let mut map = HashMap::<String, Box<Type>>::new();
                        map.insert(
                            "a".to_string(),
                            Box::new(Type {
                                shape: Box::new(ShapeType::Reference("Int".to_string())),
                                security: Box::new(SecurityType::Top),
                            }),
                        );
                        map.insert(
                            "b".to_string(),
                            Box::new(Type {
                                shape: Box::new(ShapeType::Reference("Int".to_string())),
                                security: Box::new(SecurityType::Bottom),
                            }),
                        );

                        map
                    },
                    ret: ("ret".to_string(), Box::new("Unit".parse::<Type>().unwrap())),
                    body: Box::new(Expression::IntLiteral(1)),
                }),
            ),
            (
                "fn abc(a: A {}, b: B) -> Int = 1;",
                Definition::Function(FunctionDecl {
                    doc: None,
                    name: "abc".to_string(),
                    parameters: {
                        let mut map = HashMap::<String, Box<Type>>::new();
                        map.insert(
                            "a".to_string(),
                            Box::new(Type {
                                shape: Box::new(ShapeType::Reference("A".to_string())),
                                security: Box::new(SecurityType::Bottom),
                            }),
                        );
                        map.insert(
                            "b".to_string(),
                            Box::new(Type {
                                shape: Box::new(ShapeType::Reference("B".to_string())),
                                security: Box::new(SecurityType::Top),
                            }),
                        );

                        map
                    },
                    ret: ("ret".to_string(), Box::new("Int".parse::<Type>().unwrap())),
                    body: Box::new(Expression::IntLiteral(1)),
                }),
            ),
        ];

        for (test_val, ans) in test_values {
            let (i, f) = function_decl(test_val).unwrap();

            assert_eq!(f, ans);
            assert_eq!(i, "");
        }
    }
}
