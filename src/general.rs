use nom::{character::complete::alpha1, IResult};

pub fn identifier<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    alpha1(input)
}
