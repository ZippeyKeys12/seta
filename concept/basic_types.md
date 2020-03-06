# Basic Types

The type system of Wappa is a object-oriented and functional
split, with two paths of implmentation and conversions between
the two styles.

## Tuples

Tuples are immutable sequences where the various elements can
be different types.

```Typescript
type TupleType = ( Type1, Type2 )
```

## Records

Records are effectively named tuples, and are the basic container
for fields, with every other field-type using them internally.

```Typescript
type RecordType = ( field1Name: Type1, field2Name: Type2 )
```

## Function Types

Functions have types too, corresponding to their parameter and
return types.

```Typescript
type FunctionType = (ParameterType1, ParameterType2) -> ReturnType
```

## Top and Bottom Types

A top type is a type that is the supertype of every type. In
Wappa, the top type is called, `Anything`.

A bottom type is a type that is the subtype of every type, In
Wappa, the bottom type is called, `Nothing`.

## Aliases

An alias allows one to name a type expression or literal.

```Typescript
type TypeAlias = Type1
```
