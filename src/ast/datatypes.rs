use num_bigint::BigInt;
use std::{
    cmp::{max, min},
    collections::HashMap,
    ops::{BitAnd, BitOr, BitXor, Mul, Neg, Shr, Sub},
};

#[derive(Clone, Debug, Eq, PartialEq)]
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
    Intersection(Box<DataType>, Box<DataType>),
    Union(Box<DataType>, Box<DataType>),
    Difference(Box<DataType>, Box<DataType>),
    SymmetricDifference(Box<DataType>, Box<DataType>),

    // Bounds
    Top,
    Bottom,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IntDataType {
    Unbound,
    UpperBound(BigInt),
    LowerBound(BigInt),
    BoundedRange(BigInt, BigInt),
}

impl DataType {
    fn is_subtype(&self, other: &Self) -> bool {
        match (self, other) {
            // Primitive
            (Self::Int(_), Self::Int(IntDataType::Unbound)) => true,
            (
                Self::Int(IntDataType::UpperBound(high1)),
                Self::Int(IntDataType::UpperBound(high2)),
            ) => high1 <= high2,
            (
                Self::Int(IntDataType::LowerBound(low1)),
                Self::Int(IntDataType::LowerBound(low2)),
            ) => low1 >= low2,
            (
                Self::Int(IntDataType::BoundedRange(low1, high1)),
                Self::Int(IntDataType::BoundedRange(low2, high2)),
            ) => low1 >= low2 && high1 <= high2,
            (Self::Function(args1, ret1), Self::Function(args2, ret2)) => {
                args2.is_subtype(args1) && ret1.is_subtype(ret2)
            }

            // Algebraic
            (Self::Tuple(ts1), Self::Tuple(ts2)) => {
                if ts1.len() != ts2.len() {
                    return false;
                };

                Iterator::zip(ts1.iter(), ts2.iter()).all(|(a, b)| a.is_subtype(b))
            }
            (Self::Record(ts1), Self::Record(ts2)) => {
                if ts1.len() < ts2.len() {
                    return false;
                }

                for (k, v) in ts2 {
                    if !ts1.contains_key(k) {
                        return false;
                    }

                    if !v.is_subtype(&ts2[k]) {
                        return false;
                    }
                }

                true
            }
            (Self::Variant(ts1), Self::Variant(ts2)) => {
                if ts1.len() > ts2.len() {
                    return false;
                }

                for (k, v) in ts1 {
                    if !ts2.contains_key(k) {
                        return false;
                    }

                    if !v.is_subtype(&ts2[k]) {
                        return false;
                    }
                }

                true
            }

            // Set
            (Self::Complement(t1), Self::Complement(t2)) => t2.is_subtype(t1),
            (Self::Intersection(t1, t2), t3) => t1.is_subtype(t3) || t2.is_subtype(t3),
            (Self::Union(t1, t2), t3) => t1.is_subtype(t3) && t2.is_subtype(t3),
            (t1, Self::Intersection(t2, t3)) => t1.is_subtype(t2) || t1.is_subtype(t3),
            (t1, Self::Union(t2, t3)) => t1.is_subtype(t2) && t1.is_subtype(t3),

            // Bounds
            (_, Self::Top) | (Self::Bottom, _) => true,

            // Default
            _ => false,
        }
    }

    pub fn optimize(self) -> DataType {
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

                // Default
                _ => self,
            },
            Self::Function(a, b) => Self::Function(a.optimize_boxed(), b.optimize_boxed()),

            // Algebraic
            Self::Tuple(a) => Self::Tuple(a.into_iter().map(Self::optimize).collect()),
            Self::Record(a) => {
                Self::Record(a.into_iter().map(|(a, b)| (a, b.optimize())).collect())
            }
            Self::Variant(a) => {
                Self::Variant(a.into_iter().map(|(a, b)| (a, b.optimize())).collect())
            }

            // Set
            Self::Complement(a) => match *a {
                Self::Complement(a) => (*a).optimize(),

                // Default
                _ => Self::Complement(a.optimize_boxed()),
            },
            Self::Intersection(a, b) => match (*a, *b) {
                // Primitive
                (Self::Int(int_type1), Self::Int(int_type2)) => match (int_type1, int_type2) {
                    (IntDataType::Unbound, a) | (a, IntDataType::Unbound) => Self::Int(a),
                    (IntDataType::LowerBound(low1), IntDataType::LowerBound(low2)) => {
                        Self::Int(IntDataType::LowerBound(max(low1, low2)))
                    }
                    (IntDataType::UpperBound(high1), IntDataType::UpperBound(high2)) => {
                        Self::Int(IntDataType::LowerBound(min(high1, high2)))
                    }
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
                    (IntDataType::LowerBound(low1), IntDataType::BoundedRange(low2, high))
                    | (IntDataType::BoundedRange(low2, high), IntDataType::LowerBound(low1)) => {
                        let low = max(low1, low2);

                        if low <= high {
                            Self::Int(IntDataType::BoundedRange(low, high))
                        } else {
                            Self::Bottom
                        }
                    }
                    (IntDataType::UpperBound(high1), IntDataType::BoundedRange(low, high2))
                    | (IntDataType::BoundedRange(low, high2), IntDataType::UpperBound(high1)) => {
                        let high = min(high1, high2);

                        if low <= high {
                            Self::Int(IntDataType::BoundedRange(low, high))
                        } else {
                            Self::Bottom
                        }
                    }
                },

                // Algebraic
                (Self::Tuple(a), Self::Tuple(b)) => {
                    if a == b {
                        Self::Tuple(a.into_iter().map(Self::optimize).collect())
                    } else if a.len() == b.len() {
                        let mut elems = Vec::with_capacity(a.len());

                        // Ensure tuple doesn't contradict
                        for (v1, v2) in Iterator::zip(a.iter().cloned(), b.iter().cloned()) {
                            let v1 = v1.optimize();
                            let v2 = v2.optimize();

                            if v1 != v2 {
                                let elem =
                                    Self::Intersection(Box::new(v1), Box::new(v2)).optimize();

                                if elem == Self::Bottom {
                                    return Self::Bottom;
                                }

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
                    let mut map: HashMap<String, DataType> =
                        a.into_iter().map(|t| (t.0, t.1.optimize())).collect();

                    for (k, v) in b {
                        let v = v.optimize();

                        if map.contains_key(&k) {
                            if map[&k] == v {
                                continue;
                            } else {
                                let elem =
                                    Self::Intersection(Box::new(map[&k].clone()), Box::new(v))
                                        .optimize();

                                if elem == Self::Bottom {
                                    return Self::Bottom;
                                }

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
                    let mut map: HashMap<String, DataType> =
                        a.into_iter().map(|t| (t.0, t.1.optimize())).collect();

                    for (k, v) in b {
                        let v = v.optimize();

                        if map.contains_key(&k) {
                            if map[&k] == v {
                                continue;
                            } else {
                                let elem =
                                    Self::Intersection(Box::new(map[&k].clone()), Box::new(v))
                                        .optimize();

                                if elem == Self::Bottom {
                                    return Self::Bottom;
                                }

                                map.insert(k, elem);
                            }
                        } else {
                            map.insert(k, v);
                        }
                    }

                    Self::Variant(map)
                }
                (Self::Variant(_), Self::Tuple(_)) | (Self::Variant(_), Self::Record(_)) => {
                    Self::Bottom
                }

                // Set
                (a, Self::Complement(b)) | (Self::Complement(b), a) => {
                    let a = a.optimize_boxed();
                    let b = b.optimize_boxed();

                    if a.is_subtype(&b) {
                        Self::Bottom
                    } else {
                        Self::Difference(a, b).optimize()
                    }
                }

                // Bounds
                (Self::Top, t) | (t, Self::Top) => t.optimize(),
                (Self::Bottom, _) | (_, Self::Bottom) => Self::Bottom,

                // Default
                (a, b) => {
                    let a = a.optimize_boxed();
                    let b = b.optimize_boxed();

                    if a.is_subtype(&b) {
                        *a
                    } else if b.is_subtype(&a) {
                        *b
                    } else {
                        Self::Intersection(a, b)
                    }
                }
            },
            Self::Union(a, b) => match (*a, *b) {
                // Set
                (a, Self::Complement(b)) | (Self::Complement(b), a) => {
                    let a = a.optimize_boxed();
                    let b = b.optimize_boxed();

                    if a == b {
                        Self::Top
                    } else if Self::Intersection(a.clone(), b.clone()).optimize() == Self::Bottom {
                        Self::Complement(b).optimize()
                    } else {
                        Self::Union(a, Self::Complement(b).optimize_boxed())
                    }
                }

                // Default
                (a, b) => {
                    let a = a.optimize_boxed();
                    let b = b.optimize_boxed();

                    if a.is_subtype(&b) {
                        *b
                    } else if b.is_subtype(&a) {
                        *a
                    } else {
                        Self::Union(a, b)
                    }
                }
            },
            Self::Difference(a, b) => match (*a, *b) {
                // Set
                (a, Self::Complement(b)) => {
                    Self::Intersection(a.optimize_boxed(), b.optimize_boxed())
                }

                // Bounds
                (Self::Top, a) => Self::Complement(a.optimize_boxed()).optimize(),
                (a, Self::Bottom) => a.optimize(),
                (_, Self::Top) | (Self::Bottom, _) => Self::Bottom,

                // Default
                (a, b) => {
                    let a = a.optimize_boxed();
                    let b = b.optimize_boxed();

                    if a.is_subtype(&b) {
                        Self::Bottom
                    } else {
                        Self::Difference(a, b)
                    }
                }
            },
            Self::SymmetricDifference(a, b) => {
                Self::SymmetricDifference(a.optimize_boxed(), b.optimize_boxed())
            }

            // Bounds
            Self::Top | Self::Bottom => self,
        }
    }

    fn optimize_boxed(self) -> Box<DataType> {
        Box::new(self.optimize())
    }
}

// Primitive
impl Shr for DataType {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self::Output {
        Self::Function(Box::new(self), Box::new(rhs)).optimize()
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

        Self::Tuple(elems).optimize()
    }
}

#[macro_export(inner_local_macros)]
macro_rules! record {
    ( $($name:ident : $typ:expr),* ) => {{
        use std::{collections::HashMap, stringify};
        let mut map: HashMap<String, DataType> = HashMap::new();
        $(map.insert(stringify!($name).to_string(), $typ);)*
        $crate::ast::DataType::Record(map).optimize()
    }};
}

#[macro_export(inner_local_macros)]
macro_rules! variant {
    ( $($name:ident : $typ:expr),* ) => {{
        use std::{collections::HashMap, stringify};
        let mut map: HashMap<String, DataType> = HashMap::new();
        $(map.insert(stringify!($name).to_string(), $typ);)*
        $crate::ast::DataType::Variant(map).optimize()
    }};
}

// Set
impl Neg for DataType {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Self::Complement(Box::new(self)).optimize()
    }
}

impl BitAnd for DataType {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self::Intersection(Box::new(self), Box::new(rhs)).optimize()
    }
}

impl BitOr for DataType {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self::Union(Box::new(self), Box::new(rhs)).optimize()
    }
}

impl Sub for DataType {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self::Difference(Box::new(self), Box::new(rhs)).optimize()
    }
}

impl BitXor for DataType {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self::Output {
        Self::SymmetricDifference(Box::new(self), Box::new(rhs)).optimize()
    }
}

#[cfg(test)]
mod tests {
    use super::{DataType, IntDataType};

    #[test]
    fn test_function_subtype() {
        assert!((DataType::Top >> DataType::Bottom).is_subtype(
            &(DataType::Int(IntDataType::Unbound) >> DataType::Int(IntDataType::Unbound))
        ));
    }

    #[test]
    fn test_variant_subtype() {
        assert!(variant! {
            a: DataType::Int(IntDataType::Unbound)
        }
        .is_subtype(&variant! {
            a: DataType::Int(IntDataType::Unbound),
            b: DataType::Tuple(Vec::new())
        }))
    }

    #[test]
    fn test_intersection_optimize() {
        assert_eq!(
            DataType::Bottom & DataType::Int(IntDataType::Unbound),
            DataType::Bottom
        );

        assert_eq!(
            DataType::Top & DataType::Int(IntDataType::Unbound),
            DataType::Int(IntDataType::Unbound)
        );

        assert_eq!(
            -DataType::Int(IntDataType::Unbound) & DataType::Int(IntDataType::Unbound),
            DataType::Bottom
        );

        assert_eq!(
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
            }
        );

        assert_eq!(
            -DataType::Top & DataType::Int(IntDataType::Unbound),
            DataType::Bottom
        );

        assert_eq!(
            (DataType::Int(IntDataType::Unbound) >> DataType::Int(IntDataType::Unbound))
                & (DataType::Bottom >> DataType::Top),
            DataType::Function(
                Box::new(DataType::Int(IntDataType::Unbound)),
                Box::new(DataType::Int(IntDataType::Unbound))
            )
        );

        assert_eq!(
            (DataType::Int(IntDataType::Unbound) >> DataType::Tuple(Vec::new()))
                & (DataType::Tuple(Vec::new()) >> DataType::Int(IntDataType::Unbound)),
            DataType::Intersection(
                Box::new(DataType::Function(
                    Box::new(DataType::Int(IntDataType::Unbound)),
                    Box::new(DataType::Tuple(Vec::new()))
                )),
                Box::new(DataType::Function(
                    Box::new(DataType::Tuple(Vec::new())),
                    Box::new(DataType::Int(IntDataType::Unbound))
                ))
            )
        )
    }

    #[test]
    fn test_union_optimize() {
        assert_eq!(DataType::Top | DataType::Tuple(Vec::new()), DataType::Top);

        assert_eq!(
            DataType::Bottom | DataType::Int(IntDataType::Unbound),
            DataType::Int(IntDataType::Unbound)
        );
    }

    #[test]
    fn test_difference_optimize() {
        assert_eq!(
            DataType::Bottom - DataType::Int(IntDataType::Unbound),
            DataType::Bottom
        );

        assert_eq!(
            DataType::Tuple(Vec::new()) - DataType::Top,
            DataType::Bottom
        );

        assert_eq!(
            DataType::Top - DataType::Tuple(Vec::new()),
            -DataType::Tuple(Vec::new())
        );

        assert_eq!(
            DataType::Int(IntDataType::Unbound) - DataType::Bottom,
            DataType::Int(IntDataType::Unbound)
        );
    }
}
