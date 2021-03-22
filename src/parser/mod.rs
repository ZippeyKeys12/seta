mod pratt;
mod rowan;
#[macro_use]
mod types;

pub use self::rowan::{Parser, SetaLanguage};
use ::rowan::{GreenNode, SyntaxNode};
use std::fmt;

pub struct ParseResult {
    root: GreenNode,
}

impl fmt::Debug for ParseResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tmp = format!(
            "{:#?}",
            SyntaxNode::<SetaLanguage>::new_root(self.root.clone())
        );
        write!(f, "{}", &tmp[0..tmp.len() - 1])?;

        Ok(())
    }
}
