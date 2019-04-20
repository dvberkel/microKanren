module Presentation.Goals exposing (goals)

import Dict exposing (Dict)
import MicroKanren.Kernel exposing (..)


goals : Dict String ( String, Goal Int )
goals =
    Dict.empty
        |> Dict.insert "identical_5" ( "≡ t 5", callFresh (\term -> identical term (Value 5)) )
