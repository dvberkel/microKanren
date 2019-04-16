module Presentation.Parser exposing (parse)

import Presentation.Kernel as Presentation exposing (Presentation, Slide(..))


type ParseError
    = NoSlides


parse : String -> Result ParseError Presentation
parse input =
    let
        toPresentation =
            Presentation.fromList
                >> Result.fromMaybe NoSlides
    in
    input
        |> parseSlides
        |> Result.andThen toPresentation


parseSlides : String -> Result ParseError (List Slide)
parseSlides input =
    input
        |> String.split "---\n"
        |> parseMultipleSlides


parseMultipleSlides : List String -> Result ParseError (List Slide)
parseMultipleSlides inputs =
    inputs
        |> List.map parseSlide
        |> gather


parseSlide : String -> Result ParseError Slide
parseSlide input =
    Ok <| Markdown input


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
