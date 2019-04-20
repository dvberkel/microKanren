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
move(1, X, _, Z, [[X, Z]]).
move(N, X, Y, Z, P) :-
    M is (N - 1),
    move(M, X, Z, Y, P1),
    move(1, X, Y, Z, P2),
    move(M, Y, X, Z, P3),
    append(P1, P2, Q),
    append(Q, P3, P).
```

---
goal: identical_5

---

## That was Stream