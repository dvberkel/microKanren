module BabySudoku exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attribute
import Html.Events as Event
import MicroKanren
import MicroKanren.Kernel as Kernel exposing (..)


main =
    Browser.sandbox
        { init = defaultModel
        , update = update
        , view = view
        }


type alias Model =
    { a : Hint
    , b : Hint
    , c : Hint
    , d : Hint
    , e : Hint
    , f : Hint
    , g : Hint
    , h : Hint
    , i : Hint
    , j : Hint
    , k : Hint
    , l : Hint
    , m : Hint
    , n : Hint
    , o : Hint
    , p : Hint
    }


type alias Hint =
    Maybe SudokuValue


type SudokuValue
    = One
    | Two
    | Three
    | Four


emptyModel : Model
emptyModel =
    { a = Nothing 
    , b = Nothing
    , c = Nothing
    , d = Nothing
    , e = Nothing
    , f = Nothing
    , g = Nothing
    , h = Nothing
    , i = Nothing
    , j = Nothing
    , k = Nothing
    , l = Nothing
    , m = Nothing
    , n = Nothing
    , o = Nothing
    , p = Nothing
    }

defaultModel : Model
defaultModel =
    { a = Just One 
    , b = Nothing
    , c = Nothing
    , d = Nothing
    , e = Nothing
    , f = Just Three
    , g = Nothing
    , h = Nothing
    , i = Nothing
    , j = Just One
    , k = Just Four
    , l = Nothing
    , m = Nothing
    , n = Nothing
    , o = Nothing
    , p = Just Two
    }



-- UPDATE


type Message
    = Set (String -> Model -> Model) String


update : Message -> Model -> Model
update message model =
    case message of
        Set modifier input ->
            modifier input model


setA : String -> Model -> Model
setA input model =
    { model | a = stringToSudokuValue input }


setB : String -> Model -> Model
setB input model =
    { model | b = stringToSudokuValue input }


setC : String -> Model -> Model
setC input model =
    { model | c = stringToSudokuValue input }


setD : String -> Model -> Model
setD input model =
    { model | d = stringToSudokuValue input }


setE : String -> Model -> Model
setE input model =
    { model | e = stringToSudokuValue input }


setF : String -> Model -> Model
setF input model =
    { model | f = stringToSudokuValue input }


setG : String -> Model -> Model
setG input model =
    { model | g = stringToSudokuValue input }


setH : String -> Model -> Model
setH input model =
    { model | h = stringToSudokuValue input }


setI : String -> Model -> Model
setI input model =
    { model | i = stringToSudokuValue input }


setJ : String -> Model -> Model
setJ input model =
    { model | j = stringToSudokuValue input }


setK : String -> Model -> Model
setK input model =
    { model | k = stringToSudokuValue input }


setL : String -> Model -> Model
setL input model =
    { model | l = stringToSudokuValue input }


setM : String -> Model -> Model
setM input model =
    { model | m = stringToSudokuValue input }


setN : String -> Model -> Model
setN input model =
    { model | n = stringToSudokuValue input }


setO : String -> Model -> Model
setO input model =
    { model | o = stringToSudokuValue input }


setP : String -> Model -> Model
setP input model =
    { model | p = stringToSudokuValue input }



-- VIEW


view : Model -> Html Message
view model =
    Html.div []
        [ viewPuzzleInput model
        , viewPuzzle model
        ]


viewPuzzleInput : Model -> Html Message
viewPuzzleInput model =
    let
        viewInputHintRowFor =
            viewInputHintRow model
    in
    Html.table []
        [ viewInputHintRowFor [ (setA, .a), (setB, .b), (setC, .c), (setD, .d) ]
        , viewInputHintRowFor [ (setE, .e), (setF, .f), (setG, .g), (setH, .h) ]
        , viewInputHintRowFor [ (setI, .i), (setJ, .j), (setK, .k), (setL, .l) ]
        , viewInputHintRowFor [ (setM, .m), (setN, .n), (setO, .o), (setP, .p) ]
        ]


viewInputHintRow : Model -> List (String -> Model -> Model, Model -> Hint) -> Html Message
viewInputHintRow model modifiers =
    let
        hintInputViews =
            modifiers
                |> List.map (viewHintInput model)
    in
    Html.tr [] hintInputViews


viewHintInput : Model -> (String -> Model -> Model, Model -> Hint) -> Html Message
viewHintInput model (modifier, accessor) =
    let
        theHint =
            accessor model        
    in

    Html.td []
        [ Html.select [ Event.onInput <| Set modifier ]
            [ Html.option [ Attribute.selected (theHint == Nothing),Attribute.value "unknown" ] [ Html.text "unknown" ]
            , Html.option [ Attribute.selected (theHint == Just One),Attribute.value <| sudokuValueToString One ] [ Html.text <| sudokuValueToString One ]
            , Html.option [ Attribute.selected (theHint == Just Two),Attribute.value <| sudokuValueToString Two ] [ Html.text <| sudokuValueToString Two ]
            , Html.option [ Attribute.selected (theHint == Just Three) ,Attribute.value <| sudokuValueToString Three ] [ Html.text <| sudokuValueToString Three ]
            , Html.option [ Attribute.selected (theHint == Just Four), Attribute.value <| sudokuValueToString Four ] [ Html.text <| sudokuValueToString Four ]
            ]
        ]


sudokuValueToString : SudokuValue -> String
sudokuValueToString value =
    case value of
        One ->
            "1"

        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"


stringToSudokuValue : String -> Maybe SudokuValue
stringToSudokuValue input =
    case input of
        "1" ->
            Just One

        "2" ->
            Just Two

        "3" ->
            Just Three

        "4" ->
            Just Four

        _ ->
            Nothing


sudokuValueToInt : SudokuValue -> Int
sudokuValueToInt value =
    case value of
        One ->
            1

        Two ->
            2

        Three ->
            3

        Four ->
            4


viewPuzzle : Model -> Html Message
viewPuzzle model =
    let
        hintRowFor =
            viewHintRow model
    in
    Html.table []
        [ hintRowFor [ .a, .b, .c, .d ]
        , hintRowFor [ .e, .f, .g, .h ]
        , hintRowFor [ .i, .j, .k, .l ]
        , hintRowFor [ .m, .n, .o, .p ]
        ]


viewHintRow : Model -> List (Model -> Hint) -> Html Message
viewHintRow model accessors =
    let
        hintAt =
            viewHint model

        hintViews =
            accessors
                |> List.map hintAt
    in
    Html.tr [] hintViews


viewHint : Model -> (Model -> Hint) -> Html Message
viewHint model accessor =
    Html.td [] [ Html.text <| hintToString (accessor model) ]


hintToString : Hint -> String
hintToString aHint =
    aHint
        |> Maybe.map sudokuValueToString
        |> Maybe.withDefault ""


streamModel : MicroKanren.StreamModel Int
streamModel =
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
