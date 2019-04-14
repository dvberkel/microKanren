module Presentation.Debug exposing (viewKeys)

import Html exposing (Html)
import Html.Attributes as Attribute
import Keyboard exposing (Key)

viewKeys : List Key -> Html msg
viewKeys keys =
    let
        content =
            List.map viewKey keys
    in
    Html.div [ Attribute.class "keys" ] content


viewKey : Key -> Html msg
viewKey key =
    let
        text =
            Debug.toString key
    in
    Html.span [ Attribute.class "key" ] [ Html.text text ]
