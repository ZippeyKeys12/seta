# Functional Types

## Algebraic Data Types

```Haskell
data Bool = True | False
data Color = RGB (Int, Int, Int) | RGBA (Int, Int, Int, Int)
```

## Specifications

```Haskell
spec SpecName<TypeParam> = (x: TypeParam, f: TypeParam -> TypeParam)

spec type SpecType<TypeParam> = (x: TypeParam, f: TypeParam -> TypeParam)
```

## Implementations

```Rust
impl SpecName<TypeArg> for TypeName
    public x: TypeArg

    public fun f(x: TypeArg) -> TypeArg
        TypeArg();
    end
end
```
