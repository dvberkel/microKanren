module BabySudoku exposing (main)

import Browser
import MicroKanren
import MicroKanren.Kernel as Kernel exposing (..)


main =
    Browser.sandbox
        { init = init
        , update = MicroKanren.update
        , view = MicroKanren.view String.fromInt
        }


init : MicroKanren.StreamModel Int
init =
    let
        goal =
            callFresh
                (\a ->
                    callFresh
                        (\b ->
                            callFresh
                                (\c ->
                                    callFresh
                                        (\d ->
                                            callFresh
                                                (\e ->
                                                    callFresh
                                                        (\f ->
                                                            callFresh
                                                                (\g ->
                                                                    callFresh
                                                                        (\h ->
                                                                            callFresh
                                                                                (\i ->
                                                                                    callFresh
                                                                                        (\j ->
                                                                                            callFresh
                                                                                                (\k ->
                                                                                                    callFresh
                                                                                                        (\l ->
                                                                                                            callFresh
                                                                                                                (\m ->
                                                                                                                    callFresh
                                                                                                                        (\n ->
                                                                                                                            callFresh
                                                                                                                                (\o ->
                                                                                                                                    callFresh
                                                                                                                                        (\p ->
                                                                                                                                            conj
                                                                                                                                                [ hint b 4
                                                                                                                                                , hint e 1
                                                                                                                                                , hint i 3
                                                                                                                                                , hint l 2
                                                                                                                                                , hint o 3
                                                                                                                                                , sudoku a b c d e f g h i j k l m n o p
                                                                                                                                                ]
                                                                                                                                        )
                                                                                                                                )
                                                                                                                        )
                                                                                                                )
                                                                                                        )
                                                                                                )
                                                                                        )
                                                                                )
                                                                        )
                                                                )
                                                        )
                                                )
                                        )
                                )
                        )
                )
    in
    MicroKanren.streamModelFromGoal "baby sudoku" goal


hint : Term Int -> Int -> Goal Int
hint term value =
    identical term <| Value value


sudoku : Term Int -> Term Int -> Term Int -> Term Int -> Term Int -> Term Int -> Term Int -> Term Int -> Term Int -> Term Int -> Term Int -> Term Int -> Term Int -> Term Int -> Term Int -> Term Int -> Goal Int
sudoku a b c d e f g h i j k l m n o p =
    conj
        [ sudokuRow a b c d
        , sudokuRow e f g h
        , sudokuRow i j k l
        , sudokuRow m n o p
        , sudokuColumn a e i m
        , sudokuColumn b f j n
        , sudokuColumn c g k o
        , sudokuColumn d h l p
        , sudokuBlock a b e f
        , sudokuBlock c d g h
        , sudokuBlock i j k l
        , sudokuBlock m n o p
        ]


sudokuRow : Term Int -> Term Int -> Term Int -> Term Int -> Goal Int
sudokuRow =
    pairwiseDistinct [ 1, 2, 3, 4 ]


sudokuColumn : Term Int -> Term Int -> Term Int -> Term Int -> Goal Int
sudokuColumn =
    sudokuRow


sudokuBlock : Term Int -> Term Int -> Term Int -> Term Int -> Goal Int
sudokuBlock =
    sudokuRow


pairwiseDistinct : List a -> Term a -> Term a -> Term a -> Term a -> Goal a
pairwiseDistinct elements a b c d =
    conj
        [ notEqual elements a b
        , notEqual elements a c
        , notEqual elements a d
        , notEqual elements b c
        , notEqual elements b d
        , notEqual elements c d
        ]


notEqual : List a -> Term a -> Term a -> Goal a
notEqual elements left right =
    elements
        |> split
        |> List.map (asa left right)
        |> disj


asa : Term a -> Term a -> ( a, List a ) -> Goal a
asa left right ( x, xs ) =
    conjoin
        (identical left <| Value x)
        (disj <| List.map (\y -> identical right <| Value y) xs)


conj : List (Goal a) -> Goal a
conj goals =
    case goals of
        [] ->
            succeed

        g :: gs ->
            conjoin g <| conj gs


disj : List (Goal a) -> Goal a
disj goals =
    case goals of
        [] ->
            fail

        g :: gs ->
            disjoin g <| disj gs


split : List a -> List ( a, List a )
split elements =
    let
        removeFromElements x =
            List.filter ((/=) x) elements
    in
    elements
        |> List.map (\x -> ( x, removeFromElements x ))


fail : Goal a
fail state =
    Empty


succeed : Goal a
succeed state =
    unit state
