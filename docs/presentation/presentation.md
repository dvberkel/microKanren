# μKanren
## A Minimal Functional Core for Relational Programming

[paper](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf)

---

## μKanren

> a minimalist language in the [miniKanren](http://minikanren.org/) family 
> of relational (logic) programming languages. 

---

## miniKanren

> miniKanren is an embedded Domain Specific Language for logic programming.

---

## Logic Programming
### Prolog

```prolog
ancestor(X, Z) :- parent(X, Z).
ancestor(X, Z) :- parent(Y, Z), ancestor(X, Y).

related(X, Y) :- ancestor(Z, X), ancestor(Z, Y).

parent(renee, femke).
parent(renee, daan).
parent(femke, lars).
parent(femke, joost).
parent(daan, sophie).
parent(daan, robin).
parent(daan, hannah).
```

[swish](https://swish.swi-prolog.org)

---

## Plan
* Explain the article
* using Elm

---

## Plan
* Explain the article
* using Elm

## Caveats
* Elm is strongly-typed

---

## State

---

## State

```elm
type alias State a =
    { substitution : Substitution a
    , fresh : Var
    }
```

---

## State

```elm
type alias State a =
    { substitution : Substitution a
    , fresh : Var
    }
```

```elm
type alias Substitution a =
    Dict.Dict Var (Term a)

type alias Var =
    Int

type Term a
    = Variable Var
    | Value a
    | Pair ( Term a, Term a )
```

---

## Stream

---

## Stream

```elm
type Stream a
    = Empty
    | Immature (() -> Stream a)
    | Mature (State a) (Stream a)
```

---

## Stream

```elm
type Stream a
    = Empty
    | Immature (() -> Stream a)
    | Mature (State a) (Stream a)
```

```elm
type List a
    = Nil
    | Cons a (List a)
```

---

## Goal

---

## Goal

```elm
type alias Goal a =
    State a -> Stream a
```

---

```elm
callFresh (\term -> identical term (Value 5))
```

---
goal: identical_5

---

```elm
callFresh nat
```

---
goal: nat

---

## Implementation

---

## `walk`

```elm
walk : Term a -> Substitution a -> Term a
walk term substitution =
    case term of
        Variable variable ->
            case Dict.get variable substitution of
                Just value ->
                    walk value substitution

                Nothing ->
                    term

        _ ->
            term
```

---

## `identical`

```elm
identical : Term a -> Term a -> Goal a
identical left right =
    \state ->
        case unify left right state.substitution of
            Just substitution ->
                unit
                    { state
                        | substitution = substitution
                    }

            Nothing ->
                mzero

```

---

## `identical`

```elm
identical : Term a -> Term a -> Goal a
identical left right =
    \state ->
        case unify left right state.substitution of
            Just substitution ->
                unit
                    { state
                        | substitution = substitution
                    }

            Nothing ->
                mzero

```

```elm
unit : Goal a
unit state =
    Mature state Empty


mzero : Stream a
mzero =
    Empty
```

---

## `unify`

```elm
unify : Term a -> Term a -> Substitution a -> Maybe (Substitution a)
unify left right substitution =
    let
        leftWalk =
            walk left substitution

        rightWalk =
            walk right substitution
    in
    case ( leftWalk, rightWalk ) of
        -- other cases discussed in next slides
        
        _ ->
            Nothing
```

---

## `unify` -- both variables

```elm
        ( Variable leftVariable, Variable rightVariable ) ->
            if leftVariable == rightVariable then
                Just substitution

            else
                Nothing

```

---

## `unify` -- both values

```elm
        ( Value leftValue, Value rightValue ) ->
            if leftValue == rightValue then
                Just substitution

            else
                Nothing

```

---

## `unify` -- one variable

```elm
        ( Variable leftVariable, _ ) ->
            Just (extend leftVariable rightWalk substitution)

        ( _, Variable rightVariable ) ->
            Just (extend rightVariable leftWalk substitution)

```
---

## `unify` -- both pairs 

```elm
        ( Pair ( leftFirst, leftSecond ), Pair ( rightFirst, rightSecond ) ) ->
            case unify leftFirst rightFirst substitution of
                Just nextSubstitution ->
                    unify leftSecond rightSecond nextSubstitution

                Nothing ->
                    Nothing

```

---

## `callfresh`

```elm
callFresh : (Term a -> Goal a) -> Goal a
callFresh f =
    \state -> f (Variable state.fresh) { state | fresh = state.fresh + 1 }
```

---

## `disj`

```elm
disjoin : Goal a -> Goal a -> Goal a
disjoin left right =
    \state ->
        mplus (left state) (right state)
```

---

## `conj`

```elm
conjoin : Goal a -> Goal a -> Goal a
conjoin left right =
    \state ->
        bind (left state) right
```

---

## `mplus`

---

## `mplus`

### depth-first

```elm
mplus : Stream a -> Stream a -> Stream a
mplus left right =
    case left of
        Empty ->
            right

        Immature lazyStream ->
            Immature (\_ -> mplus (lazyStream ()) right)

        Mature state followingStream ->
            Mature state (mplus followingStream right)
```

---

## `mplus`

### depth-first

```elm
mplus : Stream a -> Stream a -> Stream a
mplus left right =
    case left of
        Empty ->
            right

        Immature lazyStream ->
            Immature (\_ -> mplus (lazyStream ()) right)

        Mature state followingStream ->
            Mature state (mplus followingStream right)
```

### bread-first

```elm
mplus : Stream a -> Stream a -> Stream a
mplus left right =
    case left of
        Empty ->
            right

        Immature lazyStream ->
            Immature (\_ -> mplus right (lazyStream ()))

        Mature state followingStream ->
            Mature state (mplus right followingStream)
```

---

## `bind`

---

## `bind`

```elm
bind : Stream a -> Goal a -> Stream a
bind stream goal =
    case stream of
        Empty ->
            mzero

        Immature lazyStream ->
            Immature (\_ -> bind (lazyStream ()) goal)

        Mature state followingStream ->
            let
                goalStream =
                    goal state
            in
            mplus goalStream (bind followingStream goal)
```

---

## Finished!

---

## Demo

---

```elm
callFresh (\term -> identical term (Value 5))
```

---
goal: identical_5

---

```elm
callFresh
    (\b ->
        disjoin
            (identical b (Value 5))
            (identical b (Value 6))
    )
```

---
goal: 5_or_6

---

```elm
conjoin
    (callFresh (\a -> identical a (Value 7)))
    (callFresh
        (\b ->
            disjoin
                (identical b (Value 5))
                (identical b (Value 6))
        )
    )
```

---
goal: a_and_b

