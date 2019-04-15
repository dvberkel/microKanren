module MicroKanren.UserLevel exposing (conj, disj, pull, take, zzz)

{-| Although, as demonstrated above, one can program in the core μKanren
language, a user may rightly desire a more sophisticated set of tools with which
to program and through which to view the results. A sample set of such tools is
provided.


## Helpers

@docs conj, disj, pull, take, zzz

-}

import MicroKanren.Kernel as Kernel


{-| Perform the inverse-η-delay.

Manually performing the inverse-η-delay can quickly become tedious. The user
can instead employ the below macro Zzz (pronounced "snooze") to relieve
some of the tedium.

In the paper this is a macro, which Elm does not have. So we delay the
evaluation of the goal by hiding it behind an abstraction.

-}
zzz : (() -> Kernel.Goal a) -> Kernel.Goal a
zzz goal =
    \state -> Kernel.Immature (\_ -> goal () state)


{-| Too, manually nesting calls to conj and disj can quickly grow tiresome. The
macros conj and disj introduced below provide the conjoin and disjoin of one or more
goals.
-}
conj : List (() -> Kernel.Goal a) -> Kernel.Goal a
conj goals =
    case goals of
        [] ->
            Kernel.unit

        goal :: tailGoals ->
            Kernel.conjoin (zzz goal) (conj tailGoals)


{-| Too, manually nesting calls to conj and disj can quickly grow tiresome. The
macros conj and disj introduced below provide the conjoin and disjoin of one or more
goals.
-}
disj : List (() -> Kernel.Goal a) -> Kernel.Goal a
disj goals =
    case goals of
        [] ->
            Kernel.unit

        goal :: tailGoals ->
            Kernel.disjoin (zzz goal) (disj tailGoals)


{-| Invoking an immature stream to return results needn't be performed manually.

With an operator like pull below, this could instead be done automatically.

-}
pull : Kernel.Stream a -> Kernel.Stream a
pull stream =
    case stream of
        Kernel.Immature lazyStream ->
            pull (lazyStream ())

        _ ->
            stream


{-| take pulls the first n or as many results as the stream contains, whichever is least.
-}
take : Int -> Kernel.Stream a -> List (Kernel.State a)
take n stream =
    if n == 0 then
        []

    else
        let
            p =
                pull stream
        in
        case p of
            Kernel.Empty ->
                []

            Kernel.Mature state tailStream ->
                state :: take (n - 1) tailStream

            Kernel.Immature lazyStream ->
                -- this case will not be reached because the `pull stream`
                take n (lazyStream ())
