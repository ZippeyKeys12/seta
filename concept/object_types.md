# Object-oriented Types

## Classes

A class is a collection of fields and methods to act upon them,
with fields being stored in a companion record. One can directly
access the underlying record through `$fields` and manually pass
the value into the methods, which are stored in the record, `$methods`.

```Java
class ClassType<TypeParam: * -> * > implements InterfaceType
    public fieldName: Type1
    public field2: TypeParam<Int>

    fun __init__(self)
        fieldName = defaultValue
    end

    fun ExampleMethod1(self) -> ReturnType
        self.fieldName
    end

    fun ExampleMethod2(self, value: Type1)
        self.fieldName =  value
    end
end
```

## Interfaces

An interface is a specification for a class to follow, which is
also a type.

```Java
interface InterfaceType
    fun ExampleMethod1() -> ReturnType
end
```

## Enumerations

```Java
enum EnumType
```

## Extensions

```Swift
extension TypeName : NewInterface
    fun a()
        print("a")
    end
end
```
