# Security Classes

Types can be additionally annotated with security classses and superset relations, in order to control information flow. A type annotated with `{A, B}` permits two security classes, `A` and `B`, and is the supertype of `{A}`. Values of `{A, B}` are allowed to affect values of `{A}`, but values of `{A}` are not allowed to affect values of `{A, B}`, because that would leak information to the security class `B`.
