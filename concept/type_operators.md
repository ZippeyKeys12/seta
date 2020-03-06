# Type Operators

## Kinds

A kind is the type of a type operator or literal, which can
take in types as arguments and returns a type as a result.

The basic kind is: `*`, with operators being of the form:
`* -> *`

## Intersection

The intersection operator is of kind, `(*, *) -> *` and results
in a type that contains records that are both types.

```Typescript
type IntersectionType = Type1 & Type2
```

## Union

The union operator is of kind, `(*, *) -> *` and results
in a type that contains records that are either type.

```Typescript
type UnionType = Type1 | Type2
```

## Negation

The negation operator is of kind, `* -> *` and results in a
type that contains records that are not in the given type.

```Typescript
type NegationType = !Type1
```

## Type Constructors

Type constructors are user-defined manipulations on types.
They can be of any kind, except a literal.

```Typescript
type TypeConstructor<TypeParameter: * > = (Type1, TypeParameter)
```
