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
goal: identical_5

---

## That was Stream