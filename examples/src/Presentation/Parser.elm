module Presentation.Parser exposing (Error(..), parse)

import Dict exposing (Dict)
import MicroKanren exposing (streamModelFromGoal)
import MicroKanren.Kernel exposing (..)
import Presentation.Kernel as Presentation exposing (Presentation, Slide(..))

goals : Dict String (String, Goal Int)
goals =
    Dict.empty
    |> Dict.insert "identical_5" ("callFresh (\\term -> identical term (Value 5))", callFresh (\term -> identical term (Value 5)))

type Error
    = NoSlides
    | NoGoalKnown String


parse : String -> Result Error Presentation
parse input =
    let
        toPresentation =
            Presentation.fromList
                >> Result.fromMaybe NoSlides
    in
    input
        |> parseSlides
        |> Result.andThen toPresentation



parseSlides : String -> Result Error (List Slide)
parseSlides input =
    input
        |> String.split "---\n"
        |> parseMultipleSlides


parseMultipleSlides : List String -> Result Error (List Slide)
parseMultipleSlides inputs =
    inputs
        |> List.map parseSlide
        |> gather


parseSlide : String -> Result Error Slide
parseSlide input =
    if String.startsWith "goal: " input then
        parseGoal <| String.dropLeft 6 input

    else
        Ok <| Markdown input


parseGoal : String -> Result Error Slide
parseGoal input =
    let
        maybeGoal =
            input
            |> String.split "\n"
            |> List.head
            |> Maybe.andThen (\name -> Dict.get name goals)
    in
    case maybeGoal of
        Just (description, goal) -> 
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
