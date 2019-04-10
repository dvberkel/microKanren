module MicroKanrenTest exposing (suite)

import Dict
import Expect exposing (Expectation)
import MicroKanren.Kernel exposing (..)
import MicroKanren.UserLevel exposing (take)
import Test exposing (..)


suite : Test
suite =
    describe "MicroKanren"
        [ describe "Kernel"
            [ test "identical" <|
                \_ ->
                    let
                        goal =
                            callFresh (\term -> identical term (Value 1))

                        stream =
                            goal emptyState
                    in
                    isMature stream
            ]
        , describe "UserLevel"
            [ test "take" <|
                \_ ->
                    let
                        goal =
                            callFresh (\term -> identical term (Value 1))

                        stream =
                            goal emptyState

                        state =
                            stream
                                |> take 1
                                |> List.head
                                |> Maybe.withDefault emptyState
                    in
                    binds state 0
            ]
        ]


isMature : Stream a -> Expectation
isMature stream =
    case stream of
        Mature _ _ ->
            Expect.pass

        _ ->
            Expect.fail "not a mature stream"


binds : State a -> Var -> Expectation
binds { substitution } variable =
    if Dict.member variable substitution then
        Expect.pass

    else
        Expect.fail "variable not bound"
