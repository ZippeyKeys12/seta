use super::DataType;
use z3::{
    ast::{Ast, Bool},
    Config, Context,
};

// fn from_type<'ctx>(ctx: &'ctx Context, data_type: DataType) -> Bool<'ctx> {
//     match data_type {
//         // Primitive

//         // Algebraic
//         DataType::Tuple(elems) => (0..elems.len())
//             .map(|i| has_element(ctx, &i.to_string()))
//             .reduce(|a, b| a & b)
//             .unwrap(),
//         DataType::Record(elems) => elems
//             .iter()
//             .map(|(name, _)| has_element(ctx, name))
//             .reduce(|a, b| a & b)
//             .unwrap(),
//         DataType::Variant(variants) => variants
//             .iter()
//             .map(|(name, _)| has_variant(ctx, name))
//             .reduce(|a, b| a & b)
//             .unwrap(),

//         // Set
//         DataType::Complement(typ) => !from_type(ctx, *typ),
//         DataType::Intersection(typ1, typ2) => from_type(ctx, *typ1) & from_type(ctx, *typ2),
//         DataType::Union(typ1, typ2) => from_type(ctx, *typ1) | from_type(ctx, *typ2),
//         DataType::Difference(typ1, typ2) => from_type(ctx, *typ1) & !from_type(ctx, *typ2),
//         DataType::SymmetricDifference(typ1, typ2) => {
//             from_type(ctx, (*typ1.clone() - *typ2.clone()) & (*typ2 - *typ1))
//         }

//         // Bounds
//         DataType::Top => Bool::from_bool(ctx, true),
//         DataType::Bottom => Bool::from_bool(ctx, false),
//     }
// }

fn has_element<'ctx>(ctx: &'ctx Context, name: &str) -> Bool<'ctx> {
    return Bool::new_const(ctx, format!("has_element({})", name));
}

fn has_variant<'ctx>(ctx: &'ctx Context, name: &str) -> Bool<'ctx> {
    return Bool::new_const(ctx, format!("has_variant({})", name));
}
