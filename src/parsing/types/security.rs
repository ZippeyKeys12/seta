use crate::parsing::general::identifier;

use std::{collections::HashSet, fmt, iter::FromIterator};

use nom::{branch::alt, bytes::complete::tag, multi::separated_list, sequence::delimited, IResult};

#[derive(Clone, Debug, PartialEq)]
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
            SecurityType::Top => {
                if let Err(a) = write!(f, "_") {
                    {
                        return Err(a);
                    }
                }
            }

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

            SecurityType::Bottom => (),
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
        [] => SecurityType::Bottom,
        ["_"] => SecurityType::Top,
        _ => SecurityType::Literal(HashSet::from_iter(list.iter().map(|t| t.to_string()))),
    };

    Ok((input, Box::new(tmp)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_top() {
        let (i, t) = sec_type_expr("{_}").unwrap();
        assert_eq!(*t, SecurityType::Top);
        assert_eq!(i, "");

        let (i, t) = sec_type_expr("{_ }").unwrap();
        assert_eq!(*t, SecurityType::Top);
        assert_eq!(i, "");

        let (i, t) = sec_type_expr("{ _}").unwrap();
        assert_eq!(*t, SecurityType::Top);
        assert_eq!(i, "")
    }

    #[test]
    fn test_bottom() {
        let (i, t) = sec_type_expr("{}").unwrap();
        assert_eq!(*t, SecurityType::Bottom);
        assert_eq!(i, "");

        let (i, t) = sec_type_expr("{ }").unwrap();
        assert_eq!(*t, SecurityType::Bottom);
        assert_eq!(i, "")
    }

    #[test]
    fn test_literal() {
        let (i, t) = sec_type_expr("{A,B,C}").unwrap();

        let mut tmp = HashSet::new();
        tmp.insert("A".to_string());
        tmp.insert("B".to_string());
        tmp.insert("C".to_string());

        assert_eq!(*t, SecurityType::Literal(tmp));
        assert_eq!(i, "")
    }
}
