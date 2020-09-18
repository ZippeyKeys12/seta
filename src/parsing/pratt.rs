extern crate nom;
use nom::IResult;

pub const MAX_PRECEDENCE: u16 = u16::max_value();

type Tokenizer = for<'a> fn(input: &'a str) -> IResult<&'a str, &'a str>;

type PrefixOpParser<S, T> =
    for<'a> fn(parser: &S, input: &'a str, token: &'a str) -> IResult<&'a str, T>;

type MixfixOpParser<S, T> = for<'a> fn(
    parser: &S,
    input: &'a str,
    left: T,
    token: &'a str,
    precendence: u16,
) -> IResult<&'a str, T>;

pub struct PrattParser<'r, T> {
    pub prefixes: &'r Vec<(Tokenizer, PrefixOpParser<Self, T>)>,
    pub mixfixes: &'r Vec<(u16, Tokenizer, MixfixOpParser<Self, T>)>,
}

impl<'r, T> PrattParser<'r, T> {
    pub fn parse<'a>(&self, input: &'a str) -> IResult<&'a str, T> {
        self.expression(input, 0)
    }

    pub fn expression<'a>(&self, input: &'a str, precedence: u16) -> IResult<&'a str, T> {
        for (tokenizer, parser) in self.prefixes {
            let res = tokenizer(input);

            if res.is_ok() {
                let (input, token) = res?;
                let (mut linput, mut ltoken) = parser(self, input, token)?;

                while precedence < self.get_precedence(linput) {
                    for (prec, tokenizer, parser) in self.mixfixes {
                        let res = tokenizer(linput);

                        if res.is_ok() {
                            let (input, token) = res?;

                            let (i, t) = parser(self, input, ltoken, token, *prec)?;
                            linput = i;
                            ltoken = t;
                            break;
                        }
                    }
                }

                return Ok((linput, ltoken));
            }
        }

        Err(nom::Err::Error((
            "No type expression found",
            nom::error::ErrorKind::Alt,
        )))
    }

    fn get_precedence<'a>(&self, input: &'a str) -> u16 {
        for (precedence, tokenizer, _) in self.mixfixes {
            if tokenizer(input).is_ok() {
                return *precedence;
            }
        }

        0
    }
}
