module MicroKanren.UserLevel exposing (zzz)

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
