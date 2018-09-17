module MicroKanren.UserLevel exposing (zzz, conj)

{-| Although, as demonstrated above, one can program in the core μKanren
language, a user may rightly desire a more sophisticated set of tools with which
to program and through which to view the results. A sample set of such tools is
provided.
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
        [] -> Kernel.unit

        goal :: tailGoals -> Kernel.conjoin (zzz goal) (conj tailGoals)
