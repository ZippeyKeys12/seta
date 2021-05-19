use num_bigint::BigInt;
use std::{
    cmp::{max, min},
    collections::HashMap,
    ops::{Add, BitAnd, BitOr, BitXor, Mul, Neg, Shr, Sub},
};

#[derive(Clone, Debug, Eq)]
pub enum DataType {
    // Primitive
    Singleton(String),
    Int(IntDataType),
    Function(Box<DataType>, Box<DataType>),

    // Algebraic
    Tuple(Vec<DataType>),
    Record(HashMap<String, DataType>),
    Sum(Vec<DataType>),
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
    pub fn singleton<T>(name: T) -> Self
    where
        T: ToString,
    {
        DataType::Singleton(name.to_string())
    }

    fn subtype_of(&self, other: &Self) -> bool {
        let this = self.clone().normalized();
        let other = other.clone().normalized();

        match (this, other) {
            // Primitive
            (Self::Singleton(a), Self::Singleton(b)) => a == b,
            (Self::Int(int_typ1), Self::Int(int_typ2)) => match (int_typ1, int_typ2) {
                (IntDataType::Unbound, IntDataType::Unbound)
                | (IntDataType::LowerBound(_), IntDataType::Unbound)
                | (IntDataType::UpperBound(_), IntDataType::Unbound)
                | (IntDataType::BoundedRange(_, _), IntDataType::Unbound) => true,

                (IntDataType::LowerBound(a), IntDataType::LowerBound(b))
                | (IntDataType::BoundedRange(a, _), IntDataType::LowerBound(b)) => a >= b,
                (IntDataType::UpperBound(a), IntDataType::UpperBound(b))
                | (IntDataType::BoundedRange(_, a), IntDataType::UpperBound(b)) => a <= b,
                (IntDataType::BoundedRange(a1, b1), IntDataType::BoundedRange(a2, b2)) => {
                    a1 >= a2 && b1 <= b2
                }

                // Else
                _ => false,
            },
            (Self::Function(a1, b1), Self::Function(a2, b2)) => {
                a2.subtype_of(&a1) && b1.subtype_of(&b2)
            }

            // Algebraic
            (Self::Tuple(a), Self::Tuple(b)) => {
                if a.len() == b.len() {
                    (0..a.len()).all(|i| a[i].subtype_of(&b[i]))
                } else {
                    false
                }
            }

            // Bounds
            (Self::Bottom, _) | (_, Self::Top) => true,

            // Else
            _ => false,
        }
    }

    fn to_nnf(&self) -> Self {
        match self {
            // Nove negation inwards
            Self::Complement(a) => match *a.clone() {
                // Double Negation
                Self::Complement(b) => -b,

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
                let tmp = clean_intersection(typs, Self::to_nnf);
                let tmp2: Vec<_>;
                if let Self::Intersection(tmp) = tmp {
                    tmp2 = tmp;
                } else {
                    return tmp;
                }
                let typs = tmp2;

                if typs.is_empty() {
                    panic!("Empty intersection");
                }

                if typs.len() == 1 {
                    typs[0].clone()
                } else if typs.contains(&Self::Bottom) {
                    Self::Bottom
                } else {
                    Self::Intersection(typs)
                }
            }

            Self::Union(typs) => {
                let tmp = clean_union(typs, Self::to_nnf);
                let tmp2: Vec<_>;
                if let Self::Union(tmp) = tmp {
                    tmp2 = tmp;
                } else {
                    return tmp;
                }
                let typs = tmp2;

                if typs.is_empty() {
                    panic!("Empty union");
                }

                if typs.len() == 1 {
                    typs[0].clone()
                } else if typs.contains(&Self::Top) {
                    Self::Top
                } else {
                    Self::Union(typs)
                }
            }

            // Eliminate differences and symmetric differences
            Self::Difference(a, b) => (*a.clone() & -b.clone()).to_nnf(),
            Self::SymmetricDifference(a, b) => {
                (a.clone() & -b.clone()).to_nnf() | (b.clone() & -a.clone()).to_nnf()
            }

            // Else
            _ => self.clone(),
        }
    }

    fn to_dnf(&self) -> Self {
        let this = self.to_nnf();
        match &this {
            // Primitive
            Self::Function(a, b) => a.to_dnf() >> b.to_dnf(),

            // Algebraic
            Self::Tuple(typs) => Self::Tuple(typs.into_iter().map(Self::to_dnf).collect()),
            Self::Record(typs) => Self::Record(
                typs.into_iter()
                    .map(|(n, t)| (n.clone(), t.to_dnf()))
                    .collect(),
            ),
            Self::Variant(typs) => Self::Variant(
                typs.into_iter()
                    .map(|(n, t)| (n.clone(), t.to_dnf()))
                    .collect(),
            ),

            // Sets
            Self::Complement(a) => -a.to_dnf(),
            Self::Intersection(typs) => {
                // Distribute Intersections
                let tmp = clean_intersection(typs, Self::to_dnf);
                let tmp2: Vec<_>;
                if let Self::Intersection(tmp) = tmp {
                    tmp2 = tmp;
                } else {
                    return tmp;
                }
                let mut typs = tmp2;

                if typs.is_empty() {
                    panic!("Empty union");
                }

                if typs.len() == 1 {
                    return typs[0].clone();
                } else if typs.contains(&Self::Bottom) {
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

    fn evaluate(&self) -> Self {
        match self {
            // Primitive
            Self::Int(int_type) => match int_type {
                IntDataType::BoundedRange(low, high) => {
                    if low > high {
                        Self::Bottom
                    } else {
                        self.clone()
                    }
                }

                // Else
                _ => self.clone(),
            },
            Self::Function(a, b) => a.evaluate() >> b.evaluate(),

            // Algebraic
            Self::Tuple(typs) => Self::Tuple(typs.into_iter().map(Self::evaluate).collect()),
            Self::Record(typs) => Self::Record(
                typs.into_iter()
                    .map(|(n, t)| (n.clone(), t.evaluate()))
                    .collect(),
            ),
            Self::Variant(typs) => Self::Variant(
                typs.into_iter()
                    .map(|(n, t)| (n.clone(), t.evaluate()))
                    .collect(),
            ),

            // Set
            Self::Complement(a) => {
                let a = a.evaluate();

                match a {
                    // Primitive
                    Self::Int(a) => match a {
                        IntDataType::Unbound => Self::Bottom,
                        IntDataType::LowerBound(a) => Self::Int(IntDataType::UpperBound(a - 1)),
                        IntDataType::UpperBound(a) => Self::Int(IntDataType::LowerBound(a + 1)),
                        IntDataType::BoundedRange(a, b) => {
                            Self::Int(IntDataType::UpperBound(a - 1))
                                | Self::Int(IntDataType::LowerBound(b + 1))
                        }
                    },

                    // Set
                    Self::Complement(a) => *a,

                    // Bounds
                    Self::Top => Self::Bottom,
                    Self::Bottom => Self::Top,

                    // Else
                    _ => -a,
                }
            }
            Self::Intersection(typs) => {
                let tmp = clean_intersection(typs, Self::evaluate);
                let tmp2: Vec<_>;
                if let Self::Intersection(tmp) = tmp {
                    tmp2 = tmp;
                } else {
                    return tmp;
                }
                if tmp2.is_empty() {
                    println!("{:?}", typs);
                }
                let typs = tmp2;

                if typs.is_empty() {
                    panic!("Empty intersection");
                }

                if typs.len() == 1 {
                    return typs[0].clone();
                }

                if typs.contains(&Self::Bottom) {
                    return Self::Bottom;
                }

                let mut res = typs[0].clone();
                for typ in typs {
                    res = match (res.clone(), typ.clone()) {
                        // Primitive
                        (Self::Singleton(a), Self::Singleton(b)) => {
                            if a == b {
                                Self::Singleton(a)
                            } else {
                                return Self::Bottom;
                            }
                        }
                        (Self::Singleton(x), Self::Complement(a))
                        | (Self::Complement(a), Self::Singleton(x)) => {
                            if let Self::Singleton(y) = *a {
                                if x == y {
                                    return Self::Bottom;
                                } else {
                                    Self::Singleton(x)
                                }
                            } else {
                                Self::Singleton(x)
                            }
                        }

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
                                ) => Self::Int(IntDataType::UpperBound(min(high1, high2))),
                                (IntDataType::LowerBound(low), IntDataType::UpperBound(high))
                                | (IntDataType::UpperBound(high), IntDataType::LowerBound(low)) => {
                                    if low <= high {
                                        Self::Int(IntDataType::BoundedRange(low, high))
                                    } else {
                                        return Self::Bottom;
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
                                        return Self::Bottom;
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
                                        return Self::Bottom;
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
                                        return Self::Bottom;
                                    }
                                }
                            }
                        }
                        (Self::Int(x), Self::Complement(a))
                        | (Self::Complement(a), Self::Int(x)) => {
                            if let Self::Int(_) = *a {
                                res & typ
                            } else {
                                Self::Int(x)
                            }
                        }

                        // Algebraic
                        (Self::Tuple(a), Self::Tuple(b)) => {
                            if a == b {
                                Self::Tuple(a.iter().map(Self::evaluate).collect())
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
                                return Self::Bottom;
                            }
                        }
                        (Self::Tuple(x), Self::Complement(a))
                        | (Self::Complement(a), Self::Tuple(x)) => {
                            if let Self::Tuple(_) = *a {
                                res & typ
                            } else {
                                Self::Tuple(x)
                            }
                        }
                        (Self::Tuple(_), Self::Record(_))
                        | (Self::Tuple(_), Self::Sum(_))
                        | (Self::Tuple(_), Self::Variant(_)) => return Self::Bottom,

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
                        (Self::Record(x), Self::Complement(a))
                        | (Self::Complement(a), Self::Record(x)) => {
                            if let Self::Record(_) = *a {
                                res & typ
                            } else {
                                Self::Record(x)
                            }
                        }
                        (Self::Record(_), Self::Tuple(_))
                        | (Self::Record(_), Self::Sum(_))
                        | (Self::Record(_), Self::Variant(_)) => return Self::Bottom,

                        (Self::Sum(a), Self::Sum(b)) => {
                            if a == b {
                                Self::Sum(a.iter().map(Self::evaluate).collect())
                            } else {
                                let mut elems = Vec::with_capacity(a.len());

                                // Make sum with less options first
                                let mut a = a;
                                let mut b = b;
                                if a.len() > b.len() {
                                    let tmp = a;
                                    a = b;
                                    b = tmp;
                                }
                                let a = a;
                                let b = b;

                                // Combine options
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

                                for v in &b[a.len()..] {
                                    elems.push(v.evaluate());
                                }

                                Self::Sum(elems)
                            }
                        }
                        (Self::Sum(x), Self::Complement(a))
                        | (Self::Complement(a), Self::Sum(x)) => {
                            if let Self::Sum(_) = *a {
                                res & typ
                            } else {
                                Self::Sum(x)
                            }
                        }
                        (Self::Sum(_), Self::Tuple(_))
                        | (Self::Sum(_), Self::Record(_))
                        | (Self::Sum(_), Self::Variant(_)) => return Self::Bottom,

                        (Self::Variant(a), Self::Variant(b)) => {
                            let mut map = HashMap::new();
                            let keys: Vec<_> = a
                                .keys()
                                .into_iter()
                                .filter(|k| b.contains_key(*k))
                                .cloned()
                                .collect();

                            for k in keys {
                                map.insert(k.clone(), (a[&k].clone() & b[&k].clone()).evaluate());
                            }

                            Self::Variant(map)
                        }
                        (Self::Variant(x), Self::Complement(a))
                        | (Self::Complement(a), Self::Variant(x)) => {
                            if let Self::Variant(_) = *a {
                                res & typ
                            } else {
                                Self::Variant(x)
                            }
                        }
                        (Self::Variant(_), Self::Tuple(_))
                        | (Self::Variant(_), Self::Sum(_))
                        | (Self::Variant(_), Self::Record(_)) => return Self::Bottom,

                        // Set
                        (a, Self::Complement(b)) | (Self::Complement(b), a) => {
                            let a = a.evaluate();
                            let b = b.evaluate();
                            if a == b {
                                Self::Bottom
                            // } else if (a.clone() & b.clone()).normalized() == Self::Bottom {
                            //     a
                            } else {
                                a & -b
                            }
                        }

                        // Bounds
                        (Self::Top, c) | (c, Self::Top) => c,
                        (Self::Bottom, _) | (_, Self::Bottom) => return Self::Bottom,

                        // Else
                        _ => res & typ,
                    }
                }

                res
            }
            Self::Union(typs) => {
                let tmp = clean_union(typs, Self::evaluate);
                let tmp2: Vec<_>;
                if let Self::Union(tmp) = tmp {
                    tmp2 = tmp;
                } else {
                    return tmp;
                }
                let typs = tmp2;

                if typs.is_empty() {
                    panic!("Empty union");
                }

                if typs.contains(&Self::Top) {
                    return Self::Top;
                }
                if typs.len() == 1 {
                    return typs[0].clone();
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
                            let a = a.evaluate();
                            let b = b.evaluate();
                            if a == b {
                                Self::Top
                            // } else if (a.clone() & b.clone()).normalized() == Self::Bottom {
                            //     -b
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
            Self::Difference(a, b) => {
                let a = a.evaluate();
                let b = b.evaluate();

                if a == Self::Top {
                    -b
                } else if b == Self::Bottom {
                    a
                } else if a == Self::Bottom || b == Self::Top {
                    Self::Bottom
                } else {
                    a.evaluate() & -b.evaluate()
                }
            }
            Self::SymmetricDifference(a, b) => {
                let a = a.evaluate();
                let b = b.evaluate();
                (a.clone() - b.clone()) | (b - a)
            }

            // Else
            _ => self.clone(),
        }
    }

    fn normalized(&self) -> Self {
        self.evaluate().to_dnf().evaluate()
    }
}

impl PartialEq for DataType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Primitive
            (Self::Singleton(name1), Self::Singleton(name2)) => name1 == name2,
            (Self::Int(int_typ1), Self::Int(int_typ2)) => int_typ1 == int_typ2,
            (Self::Function(a1, b1), Self::Function(a2, b2)) => a1 == a2 && b1 == b2,

            // Algebraic
            (Self::Tuple(typs1), Self::Tuple(typs2)) | (Self::Sum(typs1), Self::Sum(typs2)) => {
                typs1 == typs2
            }
            (Self::Record(typs1), Self::Record(typs2))
            | (Self::Variant(typs1), Self::Variant(typs2)) => typs1 == typs2,

            // Set
            (Self::Complement(a), Self::Complement(b)) => *a == *b,
            (Self::Union(typs1), Self::Union(typs2))
            | (Self::Intersection(typs1), Self::Intersection(typs2)) => {
                typs1.len() == typs2.len()
                    && typs1.iter().all(|t| typs2.contains(t))
                    && typs2.iter().all(|t| typs1.contains(t))
            }
            (Self::Difference(a1, b1), Self::Difference(a2, b2))
            | (Self::SymmetricDifference(a1, b1), Self::SymmetricDifference(a2, b2)) => {
                a1 == a2 && b1 == b2
            }

            // Bounds
            (Self::Top, Self::Top) | (Self::Bottom, Self::Bottom) => true,

            // Else
            _ => false,
        }
    }
}

fn clean_intersection(typs: &Vec<DataType>, func: fn(&DataType) -> DataType) -> DataType {
    let mut tmp = Vec::new();
    let typs: Vec<_> = typs.into_iter().map(func).collect();
    for typ in &typs {
        if typ == &DataType::Top || tmp.contains(typ) {
            continue;
        }

        if typ == &DataType::Bottom {
            return DataType::Bottom;
        }

        if let DataType::Complement(a) = typ {
            if tmp.contains(&**a) {
                return DataType::Bottom;
            }
        }

        if let DataType::Intersection(typs2) = typ {
            let mut typs2 = typs2.clone();
            tmp.append(&mut typs2);
        } else {
            tmp.push(typ.clone());
        }
    }

    if tmp.len() == 1 {
        tmp[0].clone()
    } else {
        DataType::Intersection(tmp)
    }
}

fn clean_union(typs: &Vec<DataType>, func: fn(&DataType) -> DataType) -> DataType {
    let mut tmp = Vec::new();
    let typs: Vec<_> = typs.into_iter().map(func).collect();
    for typ in &typs {
        if typ == &DataType::Bottom || tmp.contains(typ) {
            continue;
        }

        if typ == &DataType::Top {
            return DataType::Top;
        }

        if let DataType::Complement(a) = typ {
            if tmp.contains(&**a) {
                return DataType::Top;
            }
        }

        if let DataType::Union(typs2) = typ {
            let mut typs2 = typs2.clone();
            tmp.append(&mut typs2);
        } else {
            tmp.push(typ.clone());
        }
    }

    if tmp.len() == 1 {
        tmp[0].clone()
    } else {
        DataType::Union(tmp)
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

impl Mul<Box<DataType>> for DataType {
    type Output = Self;

    fn mul(self, rhs: Box<Self>) -> Self::Output {
        let mut elems = Vec::new();

        if let Self::Tuple(elems1) = self {
            elems.append(&mut elems1.clone())
        } else {
            elems.push(self)
        }

        if let Self::Tuple(elems2) = *rhs {
            elems.append(&mut elems2.clone())
        } else {
            elems.push(*rhs)
        }

        Self::Tuple(elems)
    }
}

impl Mul for Box<DataType> {
    type Output = DataType;

    fn mul(self, rhs: Self) -> Self::Output {
        let mut elems = Vec::new();

        if let DataType::Tuple(elems1) = *self {
            elems.append(&mut elems1.clone())
        } else {
            elems.push(*self)
        }

        if let DataType::Tuple(elems2) = *rhs {
            elems.append(&mut elems2.clone())
        } else {
            elems.push(*rhs)
        }

        DataType::Tuple(elems)
    }
}

impl Mul<DataType> for Box<DataType> {
    type Output = DataType;

    fn mul(self, rhs: DataType) -> Self::Output {
        let mut elems = Vec::new();

        if let DataType::Tuple(elems1) = *self {
            elems.append(&mut elems1.clone())
        } else {
            elems.push(*self)
        }

        if let DataType::Tuple(elems2) = rhs {
            elems.append(&mut elems2.clone())
        } else {
            elems.push(rhs)
        }

        DataType::Tuple(elems)
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

impl Add for DataType {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let mut elems = Vec::new();

        if let Self::Sum(elems1) = self {
            elems.append(&mut elems1.clone())
        } else {
            elems.push(self)
        }

        if let Self::Sum(elems2) = rhs {
            elems.append(&mut elems2.clone())
        } else {
            elems.push(rhs)
        }

        // elems.iter().for_each(|t| {
        //     let t = t.normalized();
        //     elems
        //         .iter()
        //         .for_each(|t2| assert_eq!(Self::Bottom, (t.clone() & t2.normalized()).normalized()))
        // });

        Self::Sum(elems)
    }
}

impl Add<Box<DataType>> for DataType {
    type Output = Self;

    fn add(self, rhs: Box<Self>) -> Self::Output {
        let mut elems = Vec::new();

        if let Self::Sum(elems1) = self {
            elems.append(&mut elems1.clone())
        } else {
            elems.push(self)
        }

        if let Self::Sum(elems2) = *rhs {
            elems.append(&mut elems2.clone())
        } else {
            elems.push(*rhs)
        }

        Self::Sum(elems)
    }
}

impl Add for Box<DataType> {
    type Output = DataType;

    fn add(self, rhs: Self) -> Self::Output {
        let mut elems = Vec::new();

        if let DataType::Sum(elems1) = *self {
            elems.append(&mut elems1.clone())
        } else {
            elems.push(*self)
        }

        if let DataType::Sum(elems2) = *rhs {
            elems.append(&mut elems2.clone())
        } else {
            elems.push(*rhs)
        }

        DataType::Sum(elems)
    }
}

impl Add<DataType> for Box<DataType> {
    type Output = DataType;

    fn add(self, rhs: DataType) -> Self::Output {
        let mut elems = Vec::new();

        if let DataType::Sum(elems1) = *self {
            elems.append(&mut elems1.clone())
        } else {
            elems.push(*self)
        }

        if let DataType::Sum(elems2) = rhs {
            elems.append(&mut elems2.clone())
        } else {
            elems.push(rhs)
        }

        DataType::Sum(elems)
    }
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
    use num_bigint::ToBigInt;

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
        assert_eq!(actual.normalized(), expected)
    }

    #[test]
    fn test_normalize() {
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
        );

        cmp_normalize(
            record! {
                a: DataType::Int(IntDataType::Unbound),
                b: DataType::Top
            } & record! {
                b: DataType::Int(IntDataType::Unbound),
                c: DataType::Bottom
            },
            record! {
                a: DataType::Int(IntDataType::Unbound),
                b: DataType::Int(IntDataType::Unbound),
                c: DataType::Bottom
            },
        );

        cmp_normalize(
            record! {
                a: DataType::Top
            } | variant! {
                b: DataType::Top
            },
            variant! {
                b: DataType::Top
            } | record! {
                a: DataType::Top
            },
        );

        cmp_normalize(
            ((record! {
                a: DataType::Int(IntDataType::Unbound),
                b: DataType::Top
            } & record! {
                b: DataType::Int(IntDataType::UpperBound(25.to_bigint().unwrap()))
            }) | variant! {
                a: DataType::Top,
                b: DataType::Int(IntDataType::Unbound)
            }) & (variant! {
                b: DataType::Top,
                c: DataType::Int(IntDataType::LowerBound(5.to_bigint().unwrap()))
            } | record! {
                b: DataType::Int(IntDataType::UpperBound(10.to_bigint().unwrap())),
                c: DataType::Int(IntDataType::Unbound)
            }),
            record! {
                a: DataType::Int(IntDataType::Unbound),
                b: DataType::Int(IntDataType::UpperBound(10.to_bigint().unwrap())),
                c: DataType::Int(IntDataType::Unbound)
            } | variant! {
                b: DataType::Int(IntDataType::Unbound)
            },
        );

        cmp_normalize(DataType::Top | DataType::Tuple(Vec::new()), DataType::Top);

        cmp_normalize(
            DataType::Bottom | DataType::Int(IntDataType::Unbound),
            DataType::Int(IntDataType::Unbound),
        );

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

        cmp_normalize(
            (DataType::Top
                & (DataType::Int(IntDataType::Unbound) >> DataType::Singleton("True".to_string())))
                | DataType::Bottom,
            DataType::Int(IntDataType::Unbound) >> DataType::Singleton("True".to_string()),
        );

        cmp_normalize(
            record! {
                a: DataType::Int(IntDataType::Unbound)
            } | ((DataType::singleton("True")
                | (record! {
                    a: DataType::Int(IntDataType::LowerBound(10.to_bigint().unwrap()))
                } & record! {
                    a: DataType::Int(IntDataType::UpperBound(100.to_bigint().unwrap()))
                })
                | DataType::singleton("Null")
                | DataType::singleton("False"))
                - DataType::singleton("Null")),
            record! {
                a: DataType::Int(IntDataType::Unbound)
            } | DataType::singleton("True")
                | DataType::singleton("False"),
        );
    }
}
