use crate::parsing::general::identifier;

use std::{collections::HashSet, fmt, iter::FromIterator};

use nom::{branch::alt, bytes::complete::tag, multi::separated_list, sequence::delimited, IResult};

#[derive(Debug, PartialEq)]
pub enum SecurityType {
    Top,
    Literal(HashSet<String>),
    Bottom,
}

impl fmt::Display for SecurityType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        if let Err(a) = write!(f, "{{") {
            return Err(a);
        };

        match self {
            SecurityType::Top => (),

            SecurityType::Literal(classes) => {
                if let Err(a) = write!(
                    f,
                    "{}",
                    classes
                        .iter()
                        .cloned()
                        .fold(String::new(), |mut a: String, b: String| {
                            if a.is_empty() {
                                a.push_str(&b)
                            } else {
                                a.push_str(&format!(", {}", b))
                            };
                            a
                        })
                ) {
                    return Err(a);
                }
            }

            SecurityType::Bottom => {
                if let Err(a) = write!(f, "_") {
                    {
                        return Err(a);
                    }
                }
            }
        };

        write!(f, "}}")
    }
}

pub fn sec_type_expr(input: &str) -> IResult<&str, Box<SecurityType>> {
    let (input, list) = delimited(
        ws!(tag("{")),
        separated_list(ws!(tag(",")), ws!(alt((identifier, tag("_"))))),
        ws!(tag("}")),
    )(input)?;

    let tmp = match list[..] {
        [] => SecurityType::Top,
        ["_"] => SecurityType::Bottom,
        _ => SecurityType::Literal(HashSet::from_iter(list.iter().map(|t| t.to_string()))),
    };

    Ok((input, Box::new(tmp)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_top() {
        let (i, t) = sec_type_expr("{}").unwrap();
        assert_eq!(i, "");

        assert_eq!(*t, SecurityType::Top)
    }

    #[test]
    fn test_bottom() {
        let (i, t) = sec_type_expr("{_}").unwrap();
        assert_eq!(i, "");

        assert_eq!(*t, SecurityType::Bottom)
    }

    #[test]
    fn test_literal() {
        let (i, t) = sec_type_expr("{A,B,C}").unwrap();
        assert_eq!(i, "");

        let mut tmp = HashSet::new();
        tmp.insert("A".to_string());
        tmp.insert("B".to_string());
        tmp.insert("C".to_string());

        assert_eq!(*t, SecurityType::Literal(tmp))
    }
}
