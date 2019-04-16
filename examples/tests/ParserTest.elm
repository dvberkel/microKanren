module ParserTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Presentation.Kernel exposing (slideCount)
import Presentation.Parser exposing (parse)

exampleInput : String
exampleInput = """
# Presentation
## An Elm module

---
# Yet an other slide

---
# Last slide

"""

suite : Test
suite =
    describe "parse" [
        test "slides are delimited by \"---\"" <|
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
        
    ]
