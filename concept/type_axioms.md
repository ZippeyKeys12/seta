# Type Axioms

## Operators

### Identity

`P & Anything <=> P`

`P | Nothing <=> P`

### Idempotency

`P & P <=> P`

`P | P <=> P`

### Double Negation

`~(~P) <=> P`

### Commutative

`P & Q <=> Q & P`

`P | Q <=> Q | P`

### Distributive

`(P | Q) & (P | R) <=> P | (Q & R)`

`(P & Q) | (P & R) <=> P & (Q | R)`

`(P -> Q) & (P -> R) <=> P -> (Q & R)`

`(P -> Q) | (P -> R) <=> P -> (Q | R)`

`(P -> Q) | (R -> S) <=> (P & R) -> (Q | S)`

### DeMorgan's

`~P & ~Q <=> ~(P | Q)`

`~P | ~Q <=> ~(P & Q)`

### Inverse

`P | ~P <=> Anything`

`P & ~P <=> Nothing`

### Domination

`P | Anything <=> Anything`

`P & Nothing <=> Nothing`

### Absorption

`P | (P & Q) <=> P`

`P & (P | Q) <=> P`

`P | (~P & Q) <=> P | Q`

`P & (~P | Q) <=> P & Q`

<!-- ## Generics -->

<!-- TODO: Figure out -->

<!-- ```Haskell
type P< X+ > = X -> Int
```

`P<Y> | P<Z> <=> P<Y & Z>`

```Haskell
type Q< X, Y- > = X -> Y
```

`Q<Z, A> | Q<Z, B> <=> Q<Z, A | B>` -->
