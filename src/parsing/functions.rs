use super::{
    docs::docstring,
    expression::val_expr,
    general::{identifier, line_terminator, Definition, FunctionDecl},
    types::{
        security::SecurityType, shapes::ShapeType, type_expr, type_spec, type_spec_list, Type,
        TypeSpec,
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
    let (input, (doc, (name, parameters, ret), _, body, _)) = tuple((
        opt(ws!(docstring)),
        ws!(function_sig),
        ws!(tag("=")),
        ws!(val_expr),
        opt(ws!(line_terminator)),
    ))(input)?;

    Ok((
        input,
        Definition::Function(FunctionDecl {
            doc,
            name: name.to_string(),
            parameters,
            ret,
            body,
        }),
    ))
}

type FunctionResult<'a> = IResult<&'a str, (&'a str, HashMap<String, Type>, TypeSpec)>;

pub fn function_sig(input: &str) -> FunctionResult {
    tuple((
        ws!(preceded(ws!(tag("fn")), ws!(identifier))),
        // Parameters
        ws!(delimited(ws!(tag("(")), type_spec_list, ws!(tag(")")))),
        ws!(alt((
            preceded(
                tag("->"),
                // Gets name for return value, or defaults to 'ret'
                ws!(alt((
                    type_spec,
                    map(type_expr, |i| TypeSpec("ret".to_string(), *i))
                ))),
            ),
            |i| Ok((
                i,
                TypeSpec(
                    "ret".to_string(),
                    Type {
                        shape: Box::new("Unit".parse::<ShapeType>().unwrap()),
                        security: Box::new(SecurityType::Top)
                    }
                )
            ))
        ))),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::parsing::{
        types::{security::SecurityType, shapes::ShapeType},
        Expression,
    };

    #[test]
    fn test_function_sig() {
        let test_values = vec![
            (
                "fn a() -> A",
                (
                    "a",
                    HashMap::<String, Type>::new(),
                    TypeSpec(
                        "ret".to_string(),
                        Type {
                            shape: Box::new(ShapeType::Reference("A".to_string())),
                            security: Box::new(SecurityType::Top),
                        },
                    ),
                ),
            ),
            (
                "fn b(a: A) -> B",
                (
                    "b",
                    {
                        let mut map = HashMap::<String, Type>::new();
                        map.insert(
                            "a".to_string(),
                            Type {
                                shape: Box::new(ShapeType::Reference("A".to_string())),
                                security: Box::new(SecurityType::Top),
                            },
                        );

                        map
                    },
                    TypeSpec(
                        "ret".to_string(),
                        Type {
                            shape: Box::new(ShapeType::Reference("B".to_string())),
                            security: Box::new(SecurityType::Top),
                        },
                    ),
                ),
            ),
            (
                "fn bc(a: A, c: C {}) -> rt: B",
                (
                    "bc",
                    {
                        let mut map = HashMap::<String, Type>::new();
                        map.insert(
                            "a".to_string(),
                            Type {
                                shape: Box::new(ShapeType::Reference("A".to_string())),
                                security: Box::new(SecurityType::Top),
                            },
                        );
                        map.insert(
                            "c".to_string(),
                            Type {
                                shape: Box::new(ShapeType::Reference("C".to_string())),
                                security: Box::new(SecurityType::Bottom),
                            },
                        );

                        map
                    },
                    TypeSpec(
                        "rt".to_string(),
                        Type {
                            shape: Box::new(ShapeType::Reference("B".to_string())),
                            security: Box::new(SecurityType::Top),
                        },
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
                    ret: TypeSpec("ret".to_string(), "B".parse::<Type>().unwrap()),
                    body: Box::new(Expression::IntLiteral(1)),
                }),
            ),
            (
                "fn abc(a: Int, b: Int {}) = 1",
                Definition::Function(FunctionDecl {
                    doc: None,
                    name: "abc".to_string(),
                    parameters: {
                        let mut map = HashMap::<String, Type>::new();
                        map.insert(
                            "a".to_string(),
                            Type {
                                shape: Box::new(ShapeType::Reference("Int".to_string())),
                                security: Box::new(SecurityType::Top),
                            },
                        );
                        map.insert(
                            "b".to_string(),
                            Type {
                                shape: Box::new(ShapeType::Reference("Int".to_string())),
                                security: Box::new(SecurityType::Bottom),
                            },
                        );

                        map
                    },
                    ret: TypeSpec("ret".to_string(), "Unit".parse::<Type>().unwrap()),
                    body: Box::new(Expression::IntLiteral(1)),
                }),
            ),
            (
                "fn abc(a: A {}, b: B) -> Int = 1;",
                Definition::Function(FunctionDecl {
                    doc: None,
                    name: "abc".to_string(),
                    parameters: {
                        let mut map = HashMap::<String, Type>::new();
                        map.insert(
                            "a".to_string(),
                            Type {
                                shape: Box::new(ShapeType::Reference("A".to_string())),
                                security: Box::new(SecurityType::Bottom),
                            },
                        );
                        map.insert(
                            "b".to_string(),
                            Type {
                                shape: Box::new(ShapeType::Reference("B".to_string())),
                                security: Box::new(SecurityType::Top),
                            },
                        );

                        map
                    },
                    ret: TypeSpec("ret".to_string(), "Int".parse::<Type>().unwrap()),
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
