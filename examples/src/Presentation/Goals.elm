module Presentation.Goals exposing (goals)

import Dict exposing (Dict)
import MicroKanren.Kernel exposing (..)
import MicroKanren.Util exposing (nat)

goals : Dict String ( String, Goal Int )
goals =
    Dict.empty
        |> Dict.insert "identical_5" ( "≡ t 5", callFresh (\term -> identical term (Value 5)) )
        |> Dict.insert "nat" ("nat t", callFresh (\term -> nat term))