module Presentation.Debug exposing (viewKeys)

import Html exposing (Html)
import Html.Attributes as Attribute
import Keyboard exposing (Key)

viewKeys : (Key -> String) -> List Key -> Html msg
viewKeys nameFor keys =
    let
        content =
            List.map (viewKey nameFor) keys
    in
    Html.div [ Attribute.class "keys" ] content


viewKey : (Key -> String) -> Key -> Html msg
viewKey nameFor key =
    let
        text =
            nameFor key
    in
    Html.span [ Attribute.class "key" ] [ Html.text text ]
