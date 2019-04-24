module MicroKanren.Util exposing (nat)

{-| Various utilities such

@docs nat

-}

import MicroKanren.Kernel exposing (Goal, Stream(..), Term(..), callFresh, disjoin, identical)
import MicroKanren.UserLevel exposing (zzz)


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
            (zzz (\_ -> natFrom (start + 1) term))
