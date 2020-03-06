# Metatypes

Metatypes are the types of types, they define how types behave.

## Operators

```Typescript
metatype Intersection<A, B>
    grammar `${A} & ${B}`
        match (A, B)
        case (Negation<C>, Negation<D>) => ~(C | D)
        case (_, Negation<D>) where A==D => Nothing
        case (Negation<C>, _) where B==C => Nothing
        case _ =>
            if A <: B
                A
            elsif B <: A
                B
            else
                { x | x in A && x in B }
            end
        end
    end
end

metatype Union<A, B>
    grammar `${A} | ${B}`
        match (A, B)
        case (Negation<C>, Negation<D>) => ~(C & D)
        case (_, Negation<D>) where A==D => Anything
        case (Negation<C>, _) where B==C => Anything
        case _ =>
            if A <: B
                B
            elsif B <: A
                A
            else
                { x | x in A || x in B }
            end
        end
    end
end

metatype Negation<A>
    grammar `~${A}`
        match A
        case Anything => Nothing
        case Nothing => Something
        case Negation<C> => C
        case _ => { x | !(x in A) }
        end
    end
end
```

<!-- metatype Difference<A, B>
    grammar `${A} - ${B}`
        match (A, B)
        case (_, Negation<D>) where A==D => A
        case (Negation<C>, _) where C==B => C
        case _ =>
            if A==B || A==Nothing || B==Anything
                Nothing
            elsif B==Nothing
                Something
            else
                { x | x in A || !(x in B)}
        end
    end
end
``` -->

## Subtyping

```Typescript
metatype Subtype<A>
    `{A}+` = { x | typeof(x) <: A }
end

metatype Supertype<A>
    `{A}-` = { x | typeof(x) >: A }
end
```

<!-- ## Classes

```Typescript
metatype Class<P>
    `class extends {P}` = { x }
``` -->
