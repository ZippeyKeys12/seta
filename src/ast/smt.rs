use super::{DataType, IntDataType};
use z3::{
    ast::{Bool, Int},
    Context,
};

fn from_type<'ctx>(ctx: &'ctx Context, data_type: &DataType) -> Bool<'ctx> {
    from_type_depth(ctx, data_type, 0)
}

fn from_type_depth<'ctx>(ctx: &'ctx Context, data_type: &DataType, depth: i16) -> Bool<'ctx> {
    match data_type {
        // Primitive
        DataType::Singleton(name) => Bool::from_bool(ctx, true),
        // Bool::new_const(ctx, format!("singleton({})", name.clone())),
        DataType::Int(int_type) => {
            // Bool::new_const(ctx, "sort=int")
            //     &
            match int_type {
                IntDataType::Unbound => Bool::from_bool(ctx, true),
                IntDataType::LowerBound(low) => {
                    Int::new_const(ctx, "x")
                        .ge(&Int::from_str(ctx, low.to_string().as_str()).unwrap())
                }
                IntDataType::UpperBound(high) => {
                    Int::new_const(ctx, "x")
                        .le(&Int::from_str(ctx, high.to_string().as_str()).unwrap())
                }
                IntDataType::BoundedRange(low, high) => {
                    let x = Int::new_const(ctx, "x");
                    x.ge(&Int::from_str(ctx, low.to_string().as_str()).unwrap())
                        & x.le(&Int::from_str(ctx, high.to_string().as_str()).unwrap())
                }
            }
        }
        DataType::Function(input, output) => {
            has_input(ctx, input, depth) & has_output(ctx, output, depth)
        }

        // Algebraic
        DataType::Tuple(elems) => {
            (0..elems.len())
                .map(|i| {
                    has_element(ctx, &i.to_string(), depth).iff(&from_type_depth(
                        ctx,
                        &elems[i],
                        depth + 1,
                    ))
                })
                .reduce(|a, b| a & b)
                .unwrap()
            // & Bool::new_const(ctx, "sort=tuple")
        }
        DataType::Record(elems) => {
            elems
                .iter()
                .map(|(name, typ)| {
                    has_element(ctx, name, depth).iff(&from_type_depth(ctx, typ, depth + 1))
                })
                .reduce(|a, b| a & b)
                .unwrap()
            // & Bool::new_const(ctx, "sort=record")
        }
        DataType::Variant(variants) => {
            variants
                .iter()
                .map(|(name, typ)| {
                    has_variant(ctx, name, depth).iff(&from_type_depth(ctx, typ, depth + 1))
                })
                .reduce(|a, b| a & b)
                .unwrap()
            // & Bool::new_const(ctx, "sort=variant")
        }

        // Set
        DataType::Complement(typ) => !from_type_depth(ctx, typ, depth + 1),
        DataType::Intersection(typs) => typs
            .iter()
            .map(|t| from_type_depth(ctx, t, depth))
            .reduce(|a, b| a & b)
            .unwrap(),
        DataType::Union(typs) => typs
            .iter()
            .map(|t| from_type_depth(ctx, t, depth))
            .reduce(|a, b| a | b)
            .unwrap(),
        DataType::Difference(typ1, typ2) => {
            from_type_depth(ctx, typ1, depth + 1) & !from_type_depth(ctx, typ2, depth)
        }
        DataType::SymmetricDifference(typ1, typ2) => from_type_depth(
            ctx,
            &((typ1.clone() - typ2.clone()) & (typ2.clone() - typ1.clone())),
            depth,
        ),

        // Bounds
        DataType::Top => Bool::from_bool(ctx, true),
        DataType::Bottom => Bool::from_bool(ctx, false),
    }
}

fn has_input<'ctx>(ctx: &'ctx Context, data_type: &DataType, depth: i16) -> Bool<'ctx> {
    Bool::new_const(ctx, format!("input@{}", depth)).iff(&from_type(ctx, data_type))
}

fn has_output<'ctx>(ctx: &'ctx Context, data_type: &DataType, depth: i16) -> Bool<'ctx> {
    Bool::new_const(ctx, format!("output@{}", depth)).iff(&from_type(ctx, data_type))
}

fn has_element<'ctx>(ctx: &'ctx Context, name: &str, depth: i16) -> Bool<'ctx> {
    Bool::new_const(ctx, format!("has_element({}@{})", name, depth))
}

fn has_variant<'ctx>(ctx: &'ctx Context, name: &str, depth: i16) -> Bool<'ctx> {
    Bool::new_const(ctx, format!("has_variant({}@{})", name, depth))
}

#[cfg(test)]
mod tests {
    use super::{DataType, IntDataType};
    use crate::{ast::smt::from_type, record};
    use num_bigint::ToBigInt;
    use z3::{Config, Context, SatResult, Solver};

    fn cmp_eq(actual: DataType, expected: DataType) {
        let cfg = Config::new();
        let ctx = &Context::new(&cfg);

        assert_eq!(from_type(ctx, &actual), from_type(ctx, &expected));
    }

    fn cmp_ne(actual: DataType, expected: DataType) {
        let cfg = Config::new();
        let ctx = &Context::new(&cfg);

        assert_ne!(from_type(ctx, &actual), from_type(ctx, &expected));
    }

    fn cmp_subtype(parent: DataType, child: DataType) {
        let cfg = Config::new();
        let ctx = Context::new(&cfg);
        let solver = Solver::new(&ctx);

        let parent = from_type(&ctx, &parent);
        let child = from_type(&ctx, &child);

        solver.assert(&(&child & parent.not()));

        let res = solver.check();
        if res != SatResult::Unsat {
            println!("p:{}", parent);
            println!("c:{}", child);
            println!("m:{:#?}", solver.get_model());
        }
        assert_eq!(res, SatResult::Unsat);
    }

    fn cmp_not_subtype(parent: DataType, child: DataType) {
        let cfg = Config::new();
        let ctx = Context::new(&cfg);
        let solver = Solver::new(&ctx);

        let parent = from_type(&ctx, &parent);
        let child = from_type(&ctx, &child);

        solver.assert(&(&child & parent.not()));

        let res = solver.check();
        if res != SatResult::Sat {
            println!("p:{}", parent);
            println!("c:{}", child);
            println!("m:{:#?}", solver.get_model());
        }
        assert_eq!(solver.check(), SatResult::Sat);
    }

    #[test]
    fn test_smt() {
        // Primitive
        cmp_eq(DataType::singleton("abc"), DataType::singleton("abc"));
        cmp_subtype(
            DataType::Int(IntDataType::Unbound),
            DataType::Int(IntDataType::LowerBound(10.to_bigint().unwrap())),
        );
        cmp_subtype(
            DataType::Int(IntDataType::LowerBound(10.to_bigint().unwrap())),
            DataType::Int(IntDataType::BoundedRange(
                10.to_bigint().unwrap(),
                25.to_bigint().unwrap(),
            )),
        );
        cmp_not_subtype(
            DataType::Int(IntDataType::UpperBound(10.to_bigint().unwrap())),
            DataType::Int(IntDataType::Unbound),
        );

        cmp_eq(
            DataType::singleton("a") >> DataType::singleton("b"),
            DataType::singleton("a") >> DataType::singleton("b"),
        );

        cmp_subtype(
            DataType::singleton("True") | DataType::singleton("False"),
            DataType::singleton("True"),
        );

        let True = DataType::singleton("True");
        let False = DataType::singleton("False");
        let Bool = True.clone() | False.clone();

        cmp_subtype(
            record! {
                a: DataType::singleton("A"),
                b: DataType::singleton("B")
            } | Bool.clone(),
            record! {
                a: DataType::singleton("A"),
                b: DataType::singleton("B"),
                c: DataType::singleton("C")
            },
        );

        cmp_subtype(
            record! {
                a: DataType::singleton("A"),
                b: record!{
                    c: DataType::singleton("C"),
                    d: DataType::singleton("D")
                }
            } | Bool,
            record! {
                a: DataType::singleton("A"),
                b: record! {
                    a: DataType::singleton("A2"),
                    c: DataType::singleton("C"),
                    d: DataType::singleton("D")
                }
            },
        );

        // Bounds
        cmp_eq(DataType::Top, DataType::Top);
        cmp_eq(DataType::Bottom, DataType::Bottom);

        // let cfg = &Config::new();
        // let ctx = &Context::new(cfg);
        // let solver = Solver::new(ctx);
        // let typ = DatatypeBuilder::new(ctx, "Type")
        //     .variant("Default", Vec::new())
        //     .finish()
        //     .sort;
        // let subtype = FuncDecl::new(ctx, "subtype", &[&typ, &typ], &Sort::bool(ctx));

        // let x = Datatype::new_const(ctx, "x", &typ);
        // let subtype_x_x = subtype.apply(&[&x.clone().into(), &x.clone().into()]);
        // let subtype_x_x_pat = Pattern::new(ctx, &[&subtype_x_x]);
        // let forall = forall_const(
        //     ctx,
        //     &[&x.clone().into()],
        //     &[&subtype_x_x_pat],
        //     &subtype_x_x._eq(&Bool::from_bool(ctx, true).into()),
        // );
        // solver.assert(&forall.as_bool().unwrap());

        // assert_eq!(solver.check(), SatResult::Sat)
    }
}
