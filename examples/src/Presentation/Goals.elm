module Presentation.Goals exposing (goals)

import Dict exposing (Dict)
import MicroKanren.Kernel exposing (..)
import MicroKanren.Util exposing (nat)

goals : Dict String ( String, Goal Int )
goals =
    Dict.empty
        |> Dict.insert "identical_5" ( "≡ t 5", callFresh (\term -> identical term (Value 5)) )
        |> Dict.insert "nat" ("nat t", callFresh (\term -> nat term))
        |> Dict.insert "5_or_6" ("5 v 6", callFresh (\term -> disjoin (identical term (Value 5)) (identical term (Value 6))))
        |> Dict.insert "a_and_b" ("a A b", conjoin (callFresh (\a -> identical a (Value 7))) (callFresh (\b -> disjoin (identical b (Value 5)) (identical b (Value 6)))))