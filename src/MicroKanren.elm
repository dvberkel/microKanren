module MicroKanren exposing
    ( Goal
    , State
    , Stream(..)
    , Substitution
    , Term(..)
    , Var
    , callFresh
    , identical
    , disjoin
    , conjoin
    )

{-| μKanren provides an implementation of the

> minimalist language in the [miniKanren](http://minikanren.org/) family of
> relational (logic) programming languages.

-}

import Dict


{-| _Variable_s are identified by integers.
-}
type alias Var =
    Int


{-| _Term_s are operated on by μKanren programs
-}
type Term a
    = Variable Var
    | Value a
    | Pair ( Term a, Term a )


{-| A _substition_ binds variables to terms.
-}
type alias Substitution a =
    Dict.Dict Var (Term a)


{-| A _state_ is a substitition and the next fresh variable.
-}
type alias State a =
    { substitution : Substitution a
    , fresh : Var
    }


{-| A sequences of states is a _sequence_.

It could in principle be infinite.

-}
type Stream a
    = Empty
    | Immature (() -> Stream a)
    | Mature (State a) (Stream a)


{-| a μKanren program proceeds through the application of a _goal_ to a state.

When it succeeds it returns a non-empty stream, otherwise it fails and returns
an empty stream.

-}
type alias Goal a =
    State a -> Stream a


{-| _unit_ is the trivial goal.

It turns a state into a mature state with that single state.

-}
unit : Goal a
unit state =
    Mature state Empty


{-| _mzero_ is an alias for the empty stream.
-}
mzero : Stream a
mzero =
    Empty


{-| Lookup the value of a variable in a substitution.
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


{-| _extend_ a substitution with a new binding
-}
extend : Var -> Term a -> Substitution a -> Substitution a
extend variable term substitution =
    Dict.insert variable term substitution


{-| _unify_ two terms
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


{-| _merge_ two streams to allow fair access.

If we have a stream `0, 2, 4, 6, ...` and a stream `1, 3, 5, 7, ...`, we don't
want to see all states eventually. This function interleaves both streams so
that they occur as `0, 1, 2, 3, 4, 5, 6, 7, ...`.

-}
mplus : Stream a -> Stream a -> Stream a
mplus left right =
    case left of
        Empty ->
            right

        Immature followingStream ->
            Immature (\_ -> mplus right (followingStream ()))

        Mature state followingStream ->
            Mature state (mplus right followingStream)


{-| _bind_ a goal to each state in a stream, interleaving the resulting streams.
-}
bind : Stream a -> Goal a -> Stream a
bind stream goal =
    case stream of
        Empty ->
            mzero

        Immature followingStream ->
            Immature (\_ -> bind (followingStream ()) goal)

        Mature state followingStream ->
            let
                goalStream =
                    goal state
            in
            mplus goalStream (bind followingStream goal)


{-| a goal that succeeds if the terms unify in a certain state.
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


{-| create a goal that introduces a new variable
-}
callFresh : (Term a -> Goal a) -> Goal a
callFresh f =
    \state -> f (Variable state.fresh) { state | fresh = state.fresh + 1 }


{-| create a goal that succeeds when either of the goals succeeds.
-}
disjoin : Goal a -> Goal a -> Goal a
disjoin left right =
    \state ->
        mplus (left state) (right state)


{-| create a conjunction\_ of goals.
A goal that succeeds when the right goal is achievable in the stream
generated by the left goal
-}
conjoin : Goal a -> Goal a -> Goal a
conjoin left right =
    \state ->
        bind (left state) right
