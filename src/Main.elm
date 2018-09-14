module Main exposing (main)

import Browser
import Dict
import Html
import Html.Attributes as Attribute
import Html.Events as Event
import MicroKanren.Kernel exposing (Goal, State, Stream(..), Term(..), Var, callFresh, emptyState, identical)


main =
    let
        goals =
            [ (\term -> identical term (Value 5))
                |> callFresh
            ]
    in
    Browser.sandbox
        { init = modelFromGoals goals
        , update = update
        , view = view
        }



-- Model


type alias Model a =
    { seenStates : List (State a)
    , stream : Stream a
    }


modelFromGoal : Goal a -> Model a
modelFromGoal goal =
    { seenStates = []
    , stream = goal emptyState
    }



-- Update


type Message
    = TakeFromStream


update : Message -> Model a -> Model a
update message model =
    let
        nextModel =
            case message of
                TakeFromStream ->
                    case model.stream of
                        Empty ->
                            model

                        Immature lazyStream ->
                            { model | stream = lazyStream () }

                        Mature state followingStream ->
                            { model
                                | seenStates = List.append model.seenStates [ state ]
                                , stream = followingStream
                            }
    in
    nextModel



-- View


view : Model a -> Html.Html Message
view model =
    Html.div [ Attribute.class "microkanren" ]
        [ Html.div
            [ Attribute.classList
                [ ( "states", True )
                , ( "seen", True )
                ]
            ]
            (List.map
                viewState
                model.seenStates
            )
        , viewStream model.stream
        ]


viewState : State a -> Html.Html msg
viewState state =
    Html.div [ Attribute.class "state" ]
        [ Html.span [ Attribute.class "fresh" ] [ Html.text (String.fromInt state.fresh) ]
        , Html.div
            [ Attribute.class "substitution" ]
            (List.map
                viewBinding
                (Dict.toList state.substitution)
            )
        ]


viewBinding : ( Var, Term a ) -> Html.Html msg
viewBinding ( key, value ) =
    Html.div [ Attribute.class "binding" ]
        [ Html.span [ Attribute.class "key" ] [ Html.text (String.fromInt key) ]
        , Html.span [ Attribute.class "bind" ] [ Html.text "↦" ]
        , Html.span [ Attribute.class "value" ]
            [ Html.text (termToString Debug.toString value)
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
                ++ termToString (stringify) left
                ++ ","
                ++ termToString (stringify) right
                ++ "]"


viewStream : Stream a -> Html.Html Message
viewStream stream =
    let
        button =
            Html.button
                [ Attribute.class "pull"
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
    Html.div [ Attribute.class "stream" ] content
