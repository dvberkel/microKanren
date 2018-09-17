module MicroKanren.Kernel exposing
    ( Goal
    , State
    , Stream(..)
    , Substitution
    , Term(..)
    , Var
    , callFresh
    , conjoin
    , disjoin
    , emptyState
    , identical
    )

{-| μKanren provides an implementation of the

> minimalist language in the [miniKanren](http://minikanren.org/) family of
> relational (logic) programming languages.

-}

import Dict


{-| a μKanren program proceeds through the application of a _goal_ to a state.

Goals are often understood by analogy to predicates. Whereas the application of
a predicate to an element of its domain can be either true or false, a goal
pursued in a given state can either succeed or fail . When it succeeds it
returns a non-empty stream, otherwise it fails and returns an empty stream.

-}
type alias Goal a =
    State a -> Stream a


{-| A sequences of states is a _stream_.

A goal's success may result in a sequence of (enlarged) states, which we term a
stream. The result of a μKanren program is a stream of satisfying states. The
stream may be finite or infinite, as there may be finite or infinitely many
satisfying states

-}
type Stream a
    = Empty
    | Immature (() -> Stream a)
    | Mature (State a) (Stream a)


{-| A state is a pair of a substitution (represented as an association list) and
a non-negative integer representing a fresh-variable counter.
-}
type alias State a =
    { substitution : Substitution a
    , fresh : Var
    }


{-| The empty state is a common starting point for many μKanren programs.

While in principle the user of the system may begin with any state, in practice
the user almost always begins with empty-state . empty-state is a user-level
alias for a state virtually devoid of information: the substitution is empty,
and the first variable will be indexed at 0.

-}
emptyState : State a
emptyState =
    { substitution = Dict.empty
    , fresh = 0
    }


{-| Variable indices are identified by integers.
-}
type alias Var =
    Int


{-| Terms of the language consist of variables, objects deemed identical under
eqv? , and pairs of the foregoing.
-}
type Term a
    = Variable Var
    | Value a
    | Pair ( Term a, Term a )


{-| A _substitution_ binds variables to terms.
-}
type alias Substitution a =
    Dict.Dict Var (Term a)


{-| The _walk_ operator searches for a variable's value in the
substitution.
-}
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


{-| the ext-s operator extends the substitution with a new binding.

When extending the substitution, the first argument is always a variable, and
the second is an arbitrary term. In Friedman et. al, ext-s performs a check for
circularities in the substitution; here there is no such prohibition.

-}
extend : Var -> Term a -> Substitution a -> Substitution a
extend variable term substitution =
    Dict.insert variable term substitution


{-| ≡ takes two terms as arguments and returns a goal that succeeds
if those two terms unify in the received state.
-}
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


{-| _unit_ lifts the state into a stream whose only element is that state.
-}
unit : Goal a
unit state =
    Mature state Empty


{-| If those two terms fail to unify in that state, the empty stream, _mzero_ is
returned.
-}
mzero : Stream a
mzero =
    Empty


{-| To unify two terms in a substitution, both are walked in that substitution.
If the two terms walk to the same variable, the original substitution is
returned unchanged. When one of the two terms walks to a variable, the
substitution is extended, binding the variable to which that term walks with the
value to which the other term walks. If both terms walk to pairs, the cars and
then cdrs are unified recursively, succeeding if unification succeeds in the one
and then the other. Finally, non-variable, non-pair terms unify if they are
identical under eqv? , and unification fails otherwise.
-}
unify : Term a -> Term a -> Substitution a -> Maybe (Substitution a)
unify left right substitution =
    let
        leftWalk =
            walk left substitution

        rightWalk =
            walk right substitution
    in
    case ( leftWalk, rightWalk ) of
        ( Variable leftVariable, Variable rightVariable ) ->
            if leftVariable == rightVariable then
                Just substitution

            else
                Nothing

        ( Value leftValue, Value rightValue ) ->
            if leftValue == rightValue then
                Just substitution

            else
                Nothing

        ( Variable leftVariable, _ ) ->
            Just (extend leftVariable rightWalk substitution)

        ( _, Variable rightVariable ) ->
            Just (extend rightVariable leftWalk substitution)

        ( Pair ( leftFirst, leftSecond ), Pair ( rightFirst, rightSecond ) ) ->
            case unify leftFirst rightFirst substitution of
                Just nextSubstitution ->
                    unify leftSecond rightSecond nextSubstitution

                Nothing ->
                    Nothing

        _ ->
            Nothing


{-| The call/fresh goal constructor takes a unary function f whose body is a
goal, and itself returns a goal.

This returned goal, when provided a state s/c , binds the formal parameter of f
to a new logic variable (built with the variable constructor operator var ), and
passes a state, with the substitution it originally received and a newly
incremented fresh-variable counter, c , to the goal that is the body of f.

-}
callFresh : (Term a -> Goal a) -> Goal a
callFresh f =
    \state -> f (Variable state.fresh) { state | fresh = state.fresh + 1 }


{-| The disj goal constructor takes two goals as arguments and returns a goal
that succeeds if either of the two subgoals succeed.
-}
disjoin : Goal a -> Goal a -> Goal a
disjoin left right =
    \state ->
        mplus (left state) (right state)


{-| The conj goal constructor similarly takes two goals as arguments and returns
a goal that succeeds if both goals succeed for that state.
-}
conjoin : Goal a -> Goal a -> Goal a
conjoin left right =
    \state ->
        bind (left state) right


{-| The mplus operator is responsible for merging streams. In a goal constructed
from disj , the resulting stream contains the states that result from success of
either of the two goals.
-}
mplus : Stream a -> Stream a -> Stream a
mplus left right =
    case left of
        Empty ->
            right

        Immature lazyStream ->
            Immature (\_ -> mplus right (lazyStream ()))

        Mature state followingStream ->
            Mature state (mplus right followingStream)


{-| bind receives this resulting stream and the goal g.

In bind that goal ( g ) is invoked on each element of the stream. If the stream
of results of g is empty or becomes exhausted mzero, the empty stream, is
returned. If instead the stream contains a state and potentially more, then g is
invoked on the first state. The stream which is the result of that invocation is
merged to a stream containing the invocation of the rest of the $ passed in the
second goal g.
-}
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
