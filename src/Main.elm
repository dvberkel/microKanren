module Main exposing (main)

import Browser
import Dict
import Html
import Html.Attributes as Attribute
import Html.Events as Event
import MicroKanren exposing (nat)
import MicroKanren.Kernel exposing (..)
import MicroKanren.UserLevel exposing (..)


main =
    let
        goals =
            [ ( "≡ t 5"
              , callFresh (\term -> identical term (Value 5))
              )
            , ( "a and b"
              , conjoin
                    (callFresh (\a -> identical a (Value 7)))
                    (callFresh
                        (\b ->
                            disjoin
                                (identical b (Value 5))
                                (identical b (Value 6))
                        )
                    )
              )
            , ( "nat"
              , callFresh nat
              )
            , ( "conj (nat x) (nat y) (identical x y)"
              , callFresh
                    (\x ->
                        callFresh
                            (\y ->
                                conj
                                    [ \_ -> nat x
                                    , \_ -> nat y
                                    , \_ -> identical x y
                                    ]
                            )
                    )
              )
            ]
    in
    Browser.sandbox
        { init = modelFromGoals goals
        , update = update
        , view = view
        }



-- Model


type alias Model a =
    List (StreamModel a)


type alias StreamModel a =
    { name : String
    , seenStates : List (State a)
    , stream : Stream a
    }


modelFromGoals : List ( String, Goal a ) -> Model a
modelFromGoals goals =
    List.map modelFromGoal goals


modelFromGoal : ( String, Goal a ) -> StreamModel a
modelFromGoal ( name, goal ) =
    { name = name
    , seenStates = []
    , stream = goal emptyState
    }



-- Update


type Message
    = TakeFromStream Int


update : Message -> Model a -> Model a
update message model =
    let
        nextModel =
            case message of
                TakeFromStream index ->
                    updateInPlace next model index
    in
    nextModel


next : StreamModel a -> StreamModel a
next model =
    let
        stream =
            pull model.stream
    in
    case stream of
        Mature state followingStream ->
            { model
                | seenStates = List.append model.seenStates [ state ]
                , stream = followingStream
            }

        _ ->
            { model | stream = Empty }


updateInPlace : (StreamModel a -> StreamModel a) -> Model a -> Int -> Model a
updateInPlace f model target =
    let
        map index item =
            if index == target then
                f item
            else
                item
    in
    List.indexedMap map model


-- View


view : Model a -> Html.Html Message
view model =
    let
        streamModels =
            List.indexedMap viewStreamModel model
    in
    Html.div [ Attribute.class "microKanren" ] streamModels


viewStreamModel : Int -> StreamModel a -> Html.Html Message
viewStreamModel index model =
    Html.div [ Attribute.class "stream" ]
        [ Html.div [ Attribute.class "name" ] [ Html.span [] [ Html.text model.name ] ]
        , Html.div
            [ Attribute.classList
                [ ( "states", True )
                , ( "seen", True )
                ]
            ]
            (List.map
                viewState
                model.seenStates
            )
        , viewStream index model.stream
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
                ++ termToString stringify left
                ++ ","
                ++ termToString stringify right
                ++ "]"


viewStream : Int -> Stream a -> Html.Html Message
viewStream index stream =
    let
        button =
            Html.button
                [ Attribute.class "take"
                , Event.onClick (TakeFromStream index)
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
