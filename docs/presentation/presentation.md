# μKanren
## A Minimal Functional Core for Relational Programming

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
type alias Var =
    Int

type Term a
    = Variable Var
    | Value a
    | Pair ( Term a, Term a )

type alias Substitution a =
    Dict.Dict Var (Term a)
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
goal: nat