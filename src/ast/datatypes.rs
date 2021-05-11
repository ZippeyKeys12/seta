use num_bigint::BigInt;
use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet},
    iter::FromIterator,
    ops::{BitAnd, BitOr, BitXor, Mul, Neg, Shr, Sub},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum DataTypeSort {
    Int,
    Function,
    Tuple,
    Record,
    Variant,
    Set,
    Misc,
}

#[derive(Clone, Debug, Eq)]
pub enum DataType {
    // Primitive
    Int(IntDataType),
    Function(Box<DataType>, Box<DataType>),

    // Algebraic
    Tuple(Vec<DataType>),
    Record(HashMap<String, DataType>),
    Variant(HashMap<String, DataType>),

    // Set
    Complement(Box<DataType>),
    Intersection(Vec<DataType>),
    Union(Vec<DataType>),
    Difference(Box<DataType>, Box<DataType>),
    SymmetricDifference(Box<DataType>, Box<DataType>),

    // Bounds
    Top,
    Bottom,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IntDataType {
    Unbound,
    UpperBound(BigInt),
    LowerBound(BigInt),
    BoundedRange(BigInt, BigInt),
}

impl DataType {
    fn get_sort(&self) -> DataTypeSort {
        match self {
            // Primitive
            Self::Int(_) => DataTypeSort::Int,
            Self::Function(_, _) => DataTypeSort::Function,

            // Algebraic
            Self::Tuple(_) => DataTypeSort::Tuple,
            Self::Record(_) => DataTypeSort::Record,
            Self::Variant(_) => DataTypeSort::Variant,

            // Set
            Self::Complement(_)
            | Self::Intersection(_)
            | Self::Union(_)
            | Self::Difference(_, _)
            | Self::SymmetricDifference(_, _) => DataTypeSort::Set,

            _ => DataTypeSort::Misc,
        }
    }

    fn to_nnf(self) -> Self {
        match self {
            // Nove negation inwards
            Self::Complement(a) => match *a {
                // Double Negation
                Self::Complement(b) => *b,

                // De Morgan's
                Self::Intersection(typs) => {
                    Self::Union(typs.into_iter().map(|t| (-t).to_nnf()).collect())
                }
                Self::Union(typs) => {
                    Self::Intersection(typs.into_iter().map(|t| (-t).to_nnf()).collect())
                }

                // Bounds
                Self::Top => Self::Bottom,
                Self::Bottom => Self::Top,

                // Else
                _ => -a.to_nnf(),
            },
            Self::Intersection(typs) => {
                let mut tmp = Vec::new();
                for typ in typs.into_iter().map(Self::to_nnf) {
                    if typ == Self::Top || tmp.contains(&typ) {
                        continue;
                    }

                    tmp.push(typ);
                }
                let typs = tmp;

                if typs.is_empty() {
                    panic!("Empty intersection");
                }

                if typs.contains(&Self::Bottom) {
                    Self::Bottom
                } else {
                    Self::Intersection(typs)
                }
            }

            Self::Union(typs) => {
                let mut tmp = Vec::new();
                for typ in typs.into_iter().map(Self::to_nnf) {
                    if typ == Self::Bottom || tmp.contains(&typ) {
                        continue;
                    }

                    tmp.push(typ);
                }
                let typs = tmp;

                if typs.is_empty() {
                    panic!("Empty union");
                }

                if typs.contains(&Self::Top) {
                    Self::Top
                } else {
                    Self::Union(typs)
                }
            }

            // Eliminate differences and symmetric differences
            Self::Difference(a, b) => (a & -b).to_nnf(),
            Self::SymmetricDifference(a, b) => {
                (a.clone() & -b.clone()).to_nnf() | (b & -a).to_nnf()
            }

            // Else
            _ => self,
        }
    }

    fn to_dnf(self) -> Self {
        let this = self.to_nnf();
        match this.clone() {
            // Primitive
            Self::Function(a, b) => a.to_dnf() >> b.to_dnf(),

            // Algebraic
            Self::Tuple(typs) => Self::Tuple(typs.into_iter().map(Self::to_dnf).collect()),
            Self::Record(typs) => {
                Self::Record(typs.into_iter().map(|(n, t)| (n, t.to_dnf())).collect())
            }
            Self::Variant(typs) => {
                Self::Variant(typs.into_iter().map(|(n, t)| (n, t.to_dnf())).collect())
            }

            // Sets
            Self::Complement(a) => a.to_dnf(),
            Self::Intersection(typs) => {
                // Distribute Intersections
                let mut tmp = Vec::new();
                for typ in typs.into_iter().map(Self::to_dnf) {
                    if typ == Self::Top || tmp.contains(&typ) {
                        continue;
                    }

                    tmp.push(typ);
                }
                let mut typs = tmp;

                if typs.is_empty() {
                    panic!("Empty union");
                }

                if typs.contains(&Self::Bottom) {
                    return Self::Bottom;
                }

                let union_index =
                    typs.iter()
                        .position(|t| if let Self::Union(_) = t { true } else { false });

                match union_index {
                    Some(index) => {
                        if let Self::Union(typs2) = typs[index].clone() {
                            typs.remove(index);

                            Self::Union(
                                typs2
                                    .into_iter()
                                    .map(|t| {
                                        let mut tmp = typs.clone();
                                        tmp.push(t);
                                        Self::Intersection(tmp).to_dnf()
                                    })
                                    .collect(),
                            )
                        } else {
                            panic!("Error in 'to_dnf'");
                        }
                    }
                    None => this,
                }
            }
            Self::Union(typs) => Self::Union(typs.into_iter().map(Self::to_dnf).collect()),

            // Else
            _ => this,
        }
    }

    fn evaluate(self) -> Self {
        match self.clone() {
            // Primitive
            Self::Int(int_type) => match int_type {
                IntDataType::BoundedRange(low, high) => {
                    if low > high {
                        Self::Bottom
                    } else {
                        self
                    }
                }

                // Else
                _ => self,
            },
            Self::Function(a, b) => a.evaluate() >> b.evaluate(),

            // Algebraic
            Self::Tuple(typs) => Self::Tuple(typs.into_iter().map(Self::evaluate).collect()),
            Self::Record(typs) => {
                Self::Record(typs.into_iter().map(|(n, t)| (n, t.evaluate())).collect())
            }
            Self::Variant(typs) => {
                Self::Variant(typs.into_iter().map(|(n, t)| (n, t.evaluate())).collect())
            }

            // Set
            Self::Complement(a) => {
                let a = a.evaluate();

                if a == Self::Top {
                    Self::Bottom
                } else if a == Self::Bottom {
                    Self::Top
                } else {
                    self
                }
            }
            Self::Intersection(typs) => {
                let mut tmp = Vec::new();
                for typ in typs.into_iter().map(Self::evaluate) {
                    if typ == Self::Top || tmp.contains(&typ) {
                        continue;
                    }

                    tmp.push(typ);
                }
                let typs = tmp;

                if typs.is_empty() {
                    panic!("Empty intersection");
                }

                let sorts: Vec<_> = typs.iter().map(Self::get_sort).collect();

                let first = typs[0].get_sort();
                if typs.contains(&Self::Bottom) || !sorts.iter().all(|s| *s == first) {
                    return Self::Bottom;
                }

                let mut res = typs[0].clone();
                for typ in typs {
                    res = match (res.clone(), typ.clone()) {
                        // Primitive
                        (Self::Int(int_type1), Self::Int(int_type2)) => {
                            match (int_type1, int_type2) {
                                (IntDataType::Unbound, a) | (a, IntDataType::Unbound) => {
                                    Self::Int(a)
                                }
                                (IntDataType::LowerBound(low1), IntDataType::LowerBound(low2)) => {
                                    Self::Int(IntDataType::LowerBound(max(low1, low2)))
                                }
                                (
                                    IntDataType::UpperBound(high1),
                                    IntDataType::UpperBound(high2),
                                ) => Self::Int(IntDataType::LowerBound(min(high1, high2))),
                                (IntDataType::LowerBound(low), IntDataType::UpperBound(high))
                                | (IntDataType::UpperBound(high), IntDataType::LowerBound(low)) => {
                                    if low <= high {
                                        Self::Int(IntDataType::BoundedRange(low, high))
                                    } else {
                                        Self::Bottom
                                    }
                                }
                                (
                                    IntDataType::BoundedRange(low1, high1),
                                    IntDataType::BoundedRange(low2, high2),
                                ) => {
                                    let low = max(low1, low2);
                                    let high = min(high1, high2);

                                    if low <= high {
                                        Self::Int(IntDataType::BoundedRange(low, high))
                                    } else {
                                        Self::Bottom
                                    }
                                }
                                (
                                    IntDataType::LowerBound(low1),
                                    IntDataType::BoundedRange(low2, high),
                                )
                                | (
                                    IntDataType::BoundedRange(low2, high),
                                    IntDataType::LowerBound(low1),
                                ) => {
                                    let low = max(low1, low2);

                                    if low <= high {
                                        Self::Int(IntDataType::BoundedRange(low, high))
                                    } else {
                                        Self::Bottom
                                    }
                                }
                                (
                                    IntDataType::UpperBound(high1),
                                    IntDataType::BoundedRange(low, high2),
                                )
                                | (
                                    IntDataType::BoundedRange(low, high2),
                                    IntDataType::UpperBound(high1),
                                ) => {
                                    let high = min(high1, high2);

                                    if low <= high {
                                        Self::Int(IntDataType::BoundedRange(low, high))
                                    } else {
                                        Self::Bottom
                                    }
                                }
                            }
                        }

                        // Algebraic
                        (Self::Tuple(a), Self::Tuple(b)) => {
                            if a == b {
                                Self::Tuple(a.into_iter().map(Self::evaluate).collect())
                            } else if a.len() == b.len() {
                                let mut elems = Vec::with_capacity(a.len());

                                // Ensure tuple doesn't contradict
                                for (v1, v2) in Iterator::zip(a.iter().cloned(), b.iter().cloned())
                                {
                                    let v1 = v1.evaluate();
                                    let v2 = v2.evaluate();

                                    if v1 != v2 {
                                        let elem = (v1 & v2).evaluate();
                                        elems.push(elem);
                                    } else {
                                        elems.push(v1);
                                    }
                                }

                                Self::Tuple(elems)
                            } else {
                                Self::Bottom
                            }
                        }
                        (Self::Tuple(_), Self::Record(_)) | (Self::Tuple(_), Self::Variant(_)) => {
                            Self::Bottom
                        }

                        (Self::Record(a), Self::Record(b)) => {
                            let mut map = a;

                            for (k, v) in b {
                                let v = v.evaluate();

                                if map.contains_key(&k) {
                                    if map[&k] == v {
                                        continue;
                                    } else {
                                        let elem = (map[&k].clone() & v).evaluate();
                                        map.insert(k, elem);
                                    }
                                } else {
                                    map.insert(k, v);
                                }
                            }

                            Self::Record(map)
                        }
                        (Self::Record(_), Self::Tuple(_)) | (Self::Record(_), Self::Variant(_)) => {
                            Self::Bottom
                        }

                        (Self::Variant(a), Self::Variant(b)) => {
                            let mut map = a;

                            for (k, v) in b {
                                let v = v.evaluate();

                                if map.contains_key(&k) {
                                    if map[&k] == v {
                                        continue;
                                    } else {
                                        let elem = (map[&k].clone() & v).evaluate();
                                        map.insert(k, elem);
                                    }
                                } else {
                                    map.insert(k, v);
                                }
                            }

                            Self::Variant(map)
                        }
                        (Self::Variant(_), Self::Tuple(_))
                        | (Self::Variant(_), Self::Record(_)) => Self::Bottom,

                        // Set
                        (a, Self::Complement(b)) | (Self::Complement(b), a) => {
                            if a == *b {
                                Self::Bottom
                            } else if (a.clone() & b.clone()).normalize() == Self::Bottom {
                                a
                            } else {
                                a & -b
                            }
                        }

                        // Bounds
                        (Self::Top, c) | (c, Self::Top) => c,
                        (Self::Bottom, _) | (_, Self::Bottom) => Self::Bottom,

                        // Else
                        _ => res & typ,
                    }
                }

                res
            }
            Self::Union(typs) => {
                let mut tmp = Vec::new();
                for typ in typs.into_iter().map(Self::evaluate) {
                    if typ == Self::Bottom || tmp.contains(&typ) {
                        continue;
                    }

                    tmp.push(typ);
                }
                let typs = tmp;

                if typs.is_empty() {
                    panic!("Empty union");
                }

                if typs.contains(&Self::Top) {
                    return Self::Top;
                }

                let mut res = typs[0].clone();
                for typ in typs {
                    res = match (res.clone(), typ.clone()) {
                        // Primitive
                        (Self::Int(int_type1), Self::Int(int_type2)) => {
                            match (int_type1, int_type2) {
                                (IntDataType::Unbound, a) | (a, IntDataType::Unbound) => {
                                    Self::Int(a)
                                }
                                (IntDataType::LowerBound(low1), IntDataType::LowerBound(low2)) => {
                                    Self::Int(IntDataType::LowerBound(min(low1, low2)))
                                }
                                (
                                    IntDataType::UpperBound(high1),
                                    IntDataType::UpperBound(high2),
                                ) => Self::Int(IntDataType::LowerBound(max(high1, high2))),
                                (
                                    IntDataType::BoundedRange(low1, high1),
                                    IntDataType::BoundedRange(low2, high2),
                                ) => {
                                    let hlow = max(&low1, &low2);
                                    let lhigh = min(&high1, &high2);

                                    if hlow < lhigh {
                                        res | typ
                                    } else {
                                        let low = min(low1, low2);
                                        let high = max(high1, high2);
                                        Self::Int(IntDataType::BoundedRange(low, high))
                                    }
                                }
                                (
                                    IntDataType::LowerBound(low1),
                                    IntDataType::BoundedRange(low2, high),
                                )
                                | (
                                    IntDataType::BoundedRange(low2, high),
                                    IntDataType::LowerBound(low1),
                                ) => {
                                    if low1 <= low2 {
                                        Self::Int(IntDataType::LowerBound(low1))
                                    } else if low1 <= high {
                                        Self::Int(IntDataType::LowerBound(low2))
                                    } else {
                                        res | typ
                                    }
                                }
                                (
                                    IntDataType::UpperBound(high1),
                                    IntDataType::BoundedRange(low, high2),
                                )
                                | (
                                    IntDataType::BoundedRange(low, high2),
                                    IntDataType::UpperBound(high1),
                                ) => {
                                    if high1 >= high2 {
                                        Self::Int(IntDataType::UpperBound(high1))
                                    } else if high1 >= low {
                                        Self::Int(IntDataType::UpperBound(high2))
                                    } else {
                                        res | typ
                                    }
                                }

                                // Else
                                _ => res | typ,
                            }
                        }

                        // Set
                        (a, Self::Complement(b)) | (Self::Complement(b), a) => {
                            if a.clone().normalize() == b.clone().normalize() {
                                Self::Top
                            } else if (a.clone() & b.clone()).normalize() == Self::Bottom {
                                -b
                            } else {
                                a | -b
                            }
                        }

                        // Bounds
                        (Self::Top, _) | (_, Self::Top) => Self::Top,
                        (Self::Bottom, c) | (c, Self::Bottom) => c,

                        // Else
                        _ => res | typ,
                    }
                }

                res
            }
            Self::Difference(a, b) => a.evaluate() - b.evaluate(),
            Self::SymmetricDifference(a, b) => a.evaluate() ^ b.evaluate(),

            // Else
            _ => self,
        }
    }

    fn normalize(self) -> Self {
        self.evaluate().to_nnf().to_dnf().evaluate()
    }
}

impl PartialEq for DataType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Primitive
            (Self::Int(int_typ1), Self::Int(int_typ2)) => int_typ1 == int_typ2,
            (Self::Function(a1, b1), Self::Function(a2, b2)) => a1 == a2 && b1 == b2,

            // Algebraic
            (Self::Tuple(typs1), Self::Tuple(typs2)) => typs1 == typs2,
            (Self::Record(typs1), Self::Record(typs2)) => typs1 == typs2,
            (Self::Variant(typs1), Self::Variant(typs2)) => typs1 == typs2,

            // Set
            (Self::Complement(a), Self::Complement(b)) => a == b,
            (Self::Union(typs1), Self::Union(typs2)) => {
                typs1.len() == typs2.len()
                    && typs1.iter().all(|t| typs2.contains(t))
                    && typs2.iter().all(|t| typs1.contains(t))
            }
            (Self::Intersection(typs1), Self::Intersection(typs2)) => {
                typs1.len() == typs2.len()
                    && typs1.iter().all(|t| typs2.contains(t))
                    && typs2.iter().all(|t| typs1.contains(t))
            }
            (Self::Difference(a1, b1), Self::Difference(a2, b2)) => a1 == a2 && b1 == b2,
            (Self::SymmetricDifference(a1, b1), Self::SymmetricDifference(a2, b2)) => {
                a1 == a2 && b1 == b2
            }

            // Bounds
            (Self::Top, Self::Top) | (Self::Bottom, Self::Bottom) => true,

            // Else
            _ => false,
        }
    }
}

// Primitive
impl Shr for DataType {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        Self::Function(Box::new(self), Box::new(rhs))
    }
}

// Algebraic
impl Mul for DataType {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut elems = Vec::new();

        if let Self::Tuple(elems1) = self {
            elems.append(&mut elems1.clone())
        } else {
            elems.push(self)
        }

        if let Self::Tuple(elems2) = rhs {
            elems.append(&mut elems2.clone())
        } else {
            elems.push(rhs)
        }

        Self::Tuple(elems)
    }
}

#[macro_export(inner_local_macros)]
macro_rules! record {
    ( $($name:ident : $typ:expr),* ) => {{
        use std::{collections::HashMap, stringify};
        let mut map: HashMap<String, DataType> = HashMap::new();
        $(map.insert(stringify!($name).to_string(), $typ);)*
        $crate::ast::DataType::Record(map)
    }};
}

#[macro_export(inner_local_macros)]
macro_rules! variant {
    ( $($name:ident : $typ:expr),* ) => {{
        use std::{collections::HashMap, stringify};
        let mut map: HashMap<String, DataType> = HashMap::new();
        $(map.insert(stringify!($name).to_string(), $typ);)*
        $crate::ast::DataType::Variant(map)
    }};
}

// Set
impl Neg for DataType {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::Complement(Box::new(self))
    }
}

impl Neg for Box<DataType> {
    type Output = DataType;

    fn neg(self) -> Self::Output {
        DataType::Complement(self)
    }
}

impl BitAnd for DataType {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        match self {
            Self::Intersection(typs) => {
                if typs.contains(&rhs) {
                    Self::Intersection(typs)
                } else {
                    Self::Intersection([typs, vec![rhs]].concat())
                }
            }
            _ => {
                if self == rhs {
                    self
                } else {
                    Self::Intersection(vec![self, rhs])
                }
            }
        }
    }
}

impl BitAnd<Box<DataType>> for DataType {
    type Output = Self;

    fn bitand(self, rhs: Box<Self>) -> Self::Output {
        match self {
            Self::Intersection(typs) => {
                if typs.contains(&*rhs) {
                    Self::Intersection(typs)
                } else {
                    Self::Intersection([typs, vec![*rhs]].concat())
                }
            }
            _ => {
                if self == *rhs {
                    self
                } else {
                    Self::Intersection(vec![self, *rhs])
                }
            }
        }
    }
}

impl BitAnd for Box<DataType> {
    type Output = DataType;

    fn bitand(self, rhs: Self) -> Self::Output {
        match *self {
            DataType::Intersection(typs) => {
                if typs.contains(&*rhs) {
                    DataType::Intersection(typs)
                } else {
                    DataType::Intersection([typs, vec![*rhs]].concat())
                }
            }
            _ => {
                if self == rhs {
                    *self
                } else {
                    DataType::Intersection(vec![*self, *rhs])
                }
            }
        }
    }
}

impl BitAnd<DataType> for Box<DataType> {
    type Output = DataType;

    fn bitand(self, rhs: DataType) -> Self::Output {
        match *self {
            DataType::Intersection(typs) => {
                if typs.contains(&rhs) {
                    DataType::Intersection(typs)
                } else {
                    DataType::Intersection([typs, vec![rhs]].concat())
                }
            }
            _ => {
                if *self == rhs {
                    *self
                } else {
                    DataType::Intersection(vec![*self, rhs])
                }
            }
        }
    }
}

impl BitOr for DataType {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        match self {
            Self::Union(typs) => {
                if typs.contains(&rhs) {
                    Self::Union(typs)
                } else {
                    Self::Union([typs, vec![rhs]].concat())
                }
            }
            _ => {
                if self == rhs {
                    self
                } else {
                    Self::Union(vec![self, rhs])
                }
            }
        }
    }
}

impl BitOr<Box<DataType>> for DataType {
    type Output = Self;

    fn bitor(self, rhs: Box<Self>) -> Self::Output {
        match self {
            Self::Union(typs) => {
                if typs.contains(&*rhs) {
                    Self::Union(typs)
                } else {
                    Self::Union([typs, vec![*rhs]].concat())
                }
            }
            _ => {
                if self == *rhs {
                    self
                } else {
                    Self::Union(vec![self, *rhs])
                }
            }
        }
    }
}

impl BitOr for Box<DataType> {
    type Output = DataType;

    fn bitor(self, rhs: Self) -> Self::Output {
        match *self {
            DataType::Union(typs) => {
                if typs.contains(&*rhs) {
                    DataType::Union(typs)
                } else {
                    DataType::Union([typs, vec![*rhs]].concat())
                }
            }
            _ => {
                if self == rhs {
                    *self
                } else {
                    DataType::Union(vec![*self, *rhs])
                }
            }
        }
    }
}

impl BitOr<DataType> for Box<DataType> {
    type Output = DataType;

    fn bitor(self, rhs: DataType) -> Self::Output {
        match *self {
            DataType::Union(typs) => {
                if typs.contains(&rhs) {
                    DataType::Union(typs)
                } else {
                    DataType::Union([typs, vec![rhs]].concat())
                }
            }
            _ => {
                if *self == rhs {
                    *self
                } else {
                    DataType::Union(vec![*self, rhs])
                }
            }
        }
    }
}

impl Sub for DataType {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::Difference(Box::new(self), Box::new(rhs))
    }
}

impl Sub<Box<DataType>> for DataType {
    type Output = Self;

    fn sub(self, rhs: Box<Self>) -> Self::Output {
        Self::Difference(Box::new(self), rhs)
    }
}

impl Sub for Box<DataType> {
    type Output = DataType;

    fn sub(self, rhs: Self) -> Self::Output {
        DataType::Difference(self, rhs)
    }
}

impl Sub<DataType> for Box<DataType> {
    type Output = DataType;

    fn sub(self, rhs: DataType) -> Self::Output {
        DataType::Difference(self, Box::new(rhs))
    }
}

impl BitXor for DataType {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self::SymmetricDifference(Box::new(self), Box::new(rhs))
    }
}

impl BitXor<Box<DataType>> for DataType {
    type Output = Self;

    fn bitxor(self, rhs: Box<Self>) -> Self::Output {
        Self::SymmetricDifference(Box::new(self), rhs)
    }
}

impl BitXor for Box<DataType> {
    type Output = DataType;

    fn bitxor(self, rhs: Self) -> Self::Output {
        DataType::SymmetricDifference(self, rhs)
    }
}

impl BitXor<DataType> for Box<DataType> {
    type Output = DataType;

    fn bitxor(self, rhs: DataType) -> Self::Output {
        DataType::SymmetricDifference(self, Box::new(rhs))
    }
}

#[cfg(test)]
mod tests {
    use super::{DataType, IntDataType};

    // #[test]
    // fn test_function_subtype() {
    //     assert!((DataType::Top >> DataType::Bottom).is_subtype(
    //         &(DataType::Int(IntDataType::Unbound) >> DataType::Int(IntDataType::Unbound))
    //     ));
    // }

    // #[test]
    // fn test_variant_subtype() {
    //     assert!(variant! {
    //         a: DataType::Int(IntDataType::Unbound)
    //     }
    //     .is_subtype(&variant! {
    //         a: DataType::Int(IntDataType::Unbound),
    //         b: DataType::Tuple(Vec::new())
    //     }))
    // }

    fn cmp_normalize(actual: DataType, expected: DataType) {
        assert_eq!(actual.normalize(), expected)
    }

    #[test]
    fn test_intersection_normalize() {
        cmp_normalize(
            DataType::Bottom & DataType::Int(IntDataType::Unbound),
            DataType::Bottom,
        );

        cmp_normalize(
            DataType::Top & DataType::Int(IntDataType::Unbound),
            DataType::Int(IntDataType::Unbound),
        );

        cmp_normalize(
            -DataType::Int(IntDataType::Unbound) & DataType::Int(IntDataType::Unbound),
            DataType::Bottom,
        );

        cmp_normalize(
            record! {
                a: DataType::Tuple(Vec::new()),
                b: DataType::Top
            } & record! {
                a: DataType::Top,
                b: DataType::Int(IntDataType::Unbound)
            },
            record! {
                a: DataType::Tuple(Vec::new()),
                b: DataType::Int(IntDataType::Unbound)
            },
        );

        cmp_normalize(
            -DataType::Top & DataType::Int(IntDataType::Unbound),
            DataType::Bottom,
        );

        // cmp_normalize(
        //     (DataType::Int(IntDataType::Unbound) >> DataType::Int(IntDataType::Unbound))
        //         & (DataType::Bottom >> DataType::Top),
        //     DataType::Int(IntDataType::Unbound) >> DataType::Int(IntDataType::Unbound),
        // );

        cmp_normalize(
            (DataType::Int(IntDataType::Unbound) >> DataType::Tuple(Vec::new()))
                & (DataType::Tuple(Vec::new()) >> DataType::Int(IntDataType::Unbound)),
            (DataType::Int(IntDataType::Unbound) >> DataType::Tuple(Vec::new()))
                & (DataType::Tuple(Vec::new()) >> DataType::Int(IntDataType::Unbound)),
        )
    }

    #[test]
    fn test_union_normalize() {
        cmp_normalize(DataType::Top | DataType::Tuple(Vec::new()), DataType::Top);

        cmp_normalize(
            DataType::Bottom | DataType::Int(IntDataType::Unbound),
            DataType::Int(IntDataType::Unbound),
        );
    }

    #[test]
    fn test_difference_normalize() {
        cmp_normalize(
            DataType::Bottom - DataType::Int(IntDataType::Unbound),
            DataType::Bottom,
        );

        cmp_normalize(
            DataType::Tuple(Vec::new()) - DataType::Top,
            DataType::Bottom,
        );

        cmp_normalize(
            DataType::Top - DataType::Tuple(Vec::new()),
            -DataType::Tuple(Vec::new()),
        );

        cmp_normalize(
            DataType::Int(IntDataType::Unbound) - DataType::Bottom,
            DataType::Int(IntDataType::Unbound),
        );
    }
}
