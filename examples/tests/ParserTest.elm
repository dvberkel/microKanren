module ParserTest exposing (exampleInput, suite)

import Expect exposing (Expectation)
import Presentation.Kernel as Presentation exposing (Slide(..), emptyPresentation, slideCount)
import Presentation.Parser exposing (parse)
import Test exposing (..)


exampleInput : String
exampleInput =
    """# Presentation
## An Elm module

---
# Yet an other slide

---
# Last slide

"""


suite : Test
suite =
    describe "parse"
        [ test "slides are delimited by \"---\"" <|
            \_ ->
                let
                    actual =
                        exampleInput
                            |> parse
                            |> Result.map slideCount

                    expected =
                        Ok 3
                in
                Expect.equal actual expected
        , test "first slide is Markdown" <|
            \_ ->
                let
                    actual =
                        exampleInput
                            |> parse
                            |> Result.toMaybe 

                    expected =
                        [ Markdown "# Presentation\n## An Elm module\n\n"
                        , Markdown "# Yet an other slide\n\n"
                        , Markdown "# Last slide\n\n"
                        ]
                            |> Presentation.fromList
                in
                Expect.equal actual expected
        ]
