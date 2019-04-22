module Presentation.Parser exposing (Error(..), parse)

import Dict exposing (Dict)
import MicroKanren exposing (streamModelFromGoal)
import MicroKanren.Kernel exposing (..)
import Presentation.Kernel as Presentation exposing (Presentation, Slide(..))


type Error
    = NoSlides
    | NoGoalKnown String


parse : Dict String (String, Goal Int) -> String -> Result Error Presentation
parse goals input =
    let
        toPresentation =
            Presentation.fromList
                >> Result.fromMaybe NoSlides
    in
    input
        |> parseSlides goals
        |> Result.andThen toPresentation


parseSlides : Dict String (String, Goal Int) -> String -> Result Error (List Slide)
parseSlides goals input =
    input
        |> String.split "---\n"
        |> parseMultipleSlides goals


parseMultipleSlides : Dict String (String, Goal Int) -> List String -> Result Error (List Slide)
parseMultipleSlides goals inputs =
    inputs
        |> List.map (parseSlide goals)
        |> gather


parseSlide : Dict String (String, Goal Int) -> String -> Result Error Slide
parseSlide goals input =
    if String.startsWith "goal: " input then
        parseGoal goals <| String.dropLeft 6 input

    else
        Ok <| Markdown input


parseGoal : Dict String (String, Goal Int) -> String -> Result Error Slide
parseGoal goals input =
    let
        maybeGoal =
            input
                |> String.split "\n"
                |> List.head
                |> Maybe.andThen (\name -> Dict.get name goals)
    in
    case maybeGoal of
        Just ( description, goal ) ->
            let
                streamModel =
                    streamModelFromGoal description goal
            in
            Ok <| Stream streamModel

        Nothing ->
            Err <| NoGoalKnown input


gather : List (Result e a) -> Result e (List a)
gather =
    accumulatedGather []


accumulatedGather : List a -> List (Result e a) -> Result e (List a)
accumulatedGather acc xs =
    case xs of
        [] ->
            Ok <| List.reverse acc

        (Err e) :: _ ->
            Err e

        (Ok x) :: tail ->
            accumulatedGather (x :: acc) tail
