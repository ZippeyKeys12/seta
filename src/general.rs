use nom::{character::complete::alpha1, IResult};

pub fn identifier<'a>(input: &'a str) -> IResult<&'a str, &'a str> {
    alpha1(input)
}

macro_rules! ws {
    ($x: expr) => {
        |i| delimited(space0, $x, space0)(i)
    };
}

macro_rules! symbol {
    ($x: tt) => {
        |i| tag($x)(i)
    };
}

macro_rules! padded_symbol {
    ($x: tt) => {
        ws!(symbol!($x))
    };
}
