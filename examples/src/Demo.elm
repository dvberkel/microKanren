module Demo exposing (main)

import Browser
import Dict
import Html
import Html.Attributes as Attribute
import Html.Events as Event
import MicroKanren exposing (..)
import MicroKanren.Kernel exposing (..)
import MicroKanren.UserLevel exposing (..)



-- Goal Constructors


{-| A goal that binds a variable to the natural numbers.
-}
nat : Term Int -> Goal Int
nat =
    natFrom 0


natFrom : Int -> Term Int -> Goal Int
natFrom start =
    \term ->
        disjoin
            (identical term (Value start))
            (zzz (\_ -> natFrom (start + 1) term))


main =
    let
        goals =
            [ streamModelFromGoal "≡ t 5" <| callFresh (\term -> identical term (Value 5))
            , streamModelFromGoal "a and b" <|
                conjoin
                    (callFresh (\a -> identical a (Value 7)))
                    (callFresh
                        (\b ->
                            disjoin
                                (identical b (Value 5))
                                (identical b (Value 6))
                        )
                    )
            , streamModelFromGoal "nat" <| callFresh nat
            , streamModelFromGoal "conj (nat x) (nat y) (identical x y)" <|
                callFresh
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
            ]
    in
    Browser.sandbox
        { init = goals
        , update = update
        , view = view
        }



-- Model


type alias Model a =
    List (StreamModel a)



-- View


view : List (StreamModel a) -> Html.Html Message
view model =
    let
        map index streamView =
            Html.map (\_ -> TakeFromStream index) streamView

        streamModels =
            model
                |> List.map (MicroKanren.view Debug.toString)
                |> List.indexedMap map
    in
    Html.div [ Attribute.class "microKanren" ] streamModels



-- Update


type Message
    = TakeFromStream Int


update : Message -> Model a -> Model a
update message model =
    let
        nextModel =
            case message of
                TakeFromStream index ->
                    updateInPlace (MicroKanren.update MicroKanren.TakeFromStream) model index
    in
    nextModel


updateInPlace : (a -> a) -> List a -> Int -> List a
updateInPlace f model target =
    let
        map index item =
            if index == target then
                f item

            else
                item
    in
    List.indexedMap map model
