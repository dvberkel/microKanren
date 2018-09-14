module MicroKanren exposing (nat)

import MicroKanren.Kernel exposing (Goal, Term(..), callFresh, disjoin, identical)


{-| A goal that binds a variable to the natural numbers.
-}
nat : Term Int -> Goal Int
nat =
    natFrom 0


natFrom : Int -> Term Int -> Goal Int
natFrom start =
    \term ->
        disjoin
            (identical term (Value start))
            (natFrom (start + 1) term)
