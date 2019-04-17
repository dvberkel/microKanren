module Presentation.Parser exposing (parse, Error(..))

import Presentation.Kernel as Presentation exposing (Presentation, Slide(..))


type Error
    = NoSlides


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
