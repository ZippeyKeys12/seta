# Type Transformations

## Classes to Records

```Java
class ClassType implements InterfaceType
    public fieldName: Type1

    fun ExampleMethod1(self) -> ReturnType
        self.fieldName
    end

    fun ExampleMethod2(self, value: Type1) -> Self
        self with (fieldName: value)
    end
end
```

```Haskell
type ClassType = (
    $fields: (
        fieldName: Type1
    ),
    $methods: (
        ExampleMethod1: ($fields.$type) -> ReturnType,
        ExampleMethod2: ($fields.$type) -> ClassType
    )
)
```

## Interfaces to Specifications

```Typescript
interface X<T>
    f: T -> T
end
```

```Haskell
type spec X<T> = (f: T -> T)
```

## Implementations to Classes

```Rust
impl SpecName<TypeArg> for TypeName
    public x: TypeArg

    public fun f(x: TypeArg) -> TypeArg
        TypeArg();
    end
end
```

```Swift
extension TypeName implements SpecName<TypeArg>
    public x: TypeArg

    public fun f(x: TypeArg) -> TypeArg
        TypeArg();
    end
end
```

## Algebraic Data Types to Classes

```Haskell
data Color = RGB (Int, Int, Int) | RGBA (Int, Int, Int, Int)
```

```Haskell
data class RGB(r: Int, g: Int, b: Int)

data class RGBA(r: Int, g: Int, b: Int, a: Int)
    extends RGB(r, g, b)

type Color = RGB | RGBA
```
