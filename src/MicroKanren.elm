module MicroKanren exposing
    ( Message(..), StreamModel
    , streamModelFromGoal
    , update, view
    )

{-| Visualization of μKanren goals


## Types

@docs Message, StreamModel


## Constructor

@docs streamModelFromGoal


## Elm Architecture

@docs update, view

-}

import Dict
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
import MicroKanren.Kernel exposing (..)
import MicroKanren.UserLevel exposing (pull)



-- MODEL


{-| Keep track of a Stream.
-}
type alias StreamModel a =
    { name : String
    , seenStates : List (State a)
    , stream : Stream a
    }


{-| Create a StreamModel from a Goal.
-}
streamModelFromGoal : String -> Goal a -> StreamModel a
streamModelFromGoal name goal =
    { name = name
    , seenStates = []
    , stream = goal emptyState
    }



-- VIEW

{-| Represent a StreamModel.

This needs a way to visualize the generic parameter. It is provided as funtion with signature (a -> String). -}
view : (a -> String) -> StreamModel a -> Html.Html Message
view stringify model =
    Html.div [ Attribute.class "stream" ]
        [ Html.div [ Attribute.class "name" ] [ Html.span [] [ Html.text model.name ] ]
        , Html.div
            [ Attribute.classList
                [ ( "states", True )
                , ( "seen", True )
                ]
            ]
            (List.map
                (viewState stringify)
                model.seenStates
            )
        , viewStream model.stream
        ]


viewState : (a -> String) -> State a -> Html.Html msg
viewState stringify state =
    Html.div [ Attribute.class "state" ]
        [ Html.span [ Attribute.class "fresh" ] [ Html.text (String.fromInt state.fresh) ]
        , Html.div
            [ Attribute.class "substitution" ]
            (List.map
                (viewBinding stringify)
                (Dict.toList state.substitution)
            )
        ]


viewBinding : (a -> String) -> ( Var, Term a ) -> Html.Html msg
viewBinding stringify ( key, value ) =
    Html.div [ Attribute.class "binding" ]
        [ Html.span [ Attribute.class "key" ] [ Html.text (String.fromInt key) ]
        , Html.span [ Attribute.class "bind" ] [ Html.text "↦" ]
        , Html.span [ Attribute.class "value" ]
            [ Html.text (termToString stringify value)
            ]
        ]


termToString : (a -> String) -> Term a -> String
termToString stringify term =
    case term of
        Variable variable ->
            String.fromInt variable

        Value value ->
            stringify value

        Pair ( left, right ) ->
            "["
                ++ termToString stringify left
                ++ ","
                ++ termToString stringify right
                ++ "]"


viewStream : Stream a -> Html.Html Message
viewStream stream =
    let
        button =
            Html.button
                [ Attribute.class "take"
                , Event.onClick TakeFromStream
                ]
                [ Html.text "🡆" ]

        content =
            case stream of
                Empty ->
                    [ Html.span [] [ Html.text "empty" ] ]

                Immature _ ->
                    [ Html.span [] [ Html.text "immature" ]
                    , button
                    ]

                Mature _ _ ->
                    [ Html.span [] [ Html.text "mature" ]
                    , button
                    ]
    in
    Html.div [ Attribute.class "control" ] content



-- UPDATE

{-| Message a StreamModel can react to -}
type Message
    = TakeFromStream

{-| Update the StreamModel according to the received Message -}
update : Message -> StreamModel a -> StreamModel a
update _ model =
    let
        stream =
            pull model.stream
    in
    case stream of
        Mature state followingStream ->
            { model
                | seenStates = model.seenStates ++ [ state ]
                , stream = followingStream
            }

        _ ->
            { model | stream = Empty }
