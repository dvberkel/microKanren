module Presentation exposing (Presentation, Slide(..))

import Browser
import Html exposing (Html)
import Html.Attributes as Attribute
import Keyboard exposing (Key(..))


main : Program () Model Message
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



{- MODEL -}


init : ( Model, Cmd Message )
init =
    let
        presentation =
            [ Blank, Blank, Blank ]
                |> fromList
                |> Maybe.withDefault emptyPresentation
    in
    ( createModel presentation, Cmd.none )


type alias Model =
    { pressedKeys : List Key
    , presentation : Presentation
    }


type Presentation
    = Presentation
        { preceding : List Slide
        , current : Slide
        , following : List Slide
        }


type Slide
    = Blank


createModel : Presentation -> Model
createModel presentation =
    { pressedKeys = [], presentation = presentation }


emptyPresentation : Presentation
emptyPresentation =
    Presentation
        { preceding = []
        , current = Blank
        , following = []
        }


fromList : List Slide -> Maybe Presentation
fromList slides =
    if List.length slides > 0 then
        let
            current =
                slides
                    |> List.head
                    |> Maybe.withDefault Blank

            following =
                slides
                    |> List.tail
                    |> Maybe.withDefault []
        in
        Presentation { preceding = [], current = current, following = following }
            |> Just

    else
        Nothing


backtrack : Presentation -> Presentation
backtrack ((Presentation data) as original) =
    if List.length data.preceding > 0 then
        let
            nextPreceding =
                data.preceding
                    |> List.tail
                    |> Maybe.withDefault []

            nextCurrent =
                data.preceding
                    |> List.head
                    |> Maybe.withDefault Blank

            nextFollowing =
                data.current :: data.following
        in
        Presentation
            { data
                | preceding = nextPreceding
                , current = nextCurrent
                , following = nextFollowing
            }

    else
        original


advance : Presentation -> Presentation
advance ((Presentation data) as original) =
    if List.length data.following > 0 then
        let
            nextPreceding =
                data.current :: data.preceding

            nextCurrent =
                data.following
                    |> List.head
                    |> Maybe.withDefault Blank

            nextFollowing =
                data.following
                    |> List.tail
                    |> Maybe.withDefault []
        in
        Presentation
            { data
                | preceding = nextPreceding
                , current = nextCurrent
                , following = nextFollowing
            }

    else
        original


currentIndex : Presentation -> Int
currentIndex (Presentation { preceding }) =
    1 + List.length preceding


slideCount : Presentation -> Int
slideCount (Presentation { preceding, current, following }) =
    List.length preceding + 1 + List.length following



{- VIEW -}


view : Model -> Html Message
view model =
    Html.div [ Attribute.class "presentation" ]
        [ viewInfo model.presentation
        ]


viewInfo : Presentation -> Html Message
viewInfo presentation =
    Html.div [ Attribute.class "info" ]
        [ viewCount presentation
        ]


viewCount : Presentation -> Html Message
viewCount presentation =
    Html.div [ Attribute.class "count" ]
        [ Html.span [ Attribute.class "index" ] [ Html.text <| String.fromInt <| currentIndex presentation ]
        , Html.span [ Attribute.class "total" ] [ Html.text <| String.fromInt <| slideCount presentation ]
        ]



{- UPDATE -}


type Message
    = KeyMessage Keyboard.Msg


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        KeyMessage keyMessage ->
            let
                nextModel =
                    { model
                        | pressedKeys = Keyboard.update keyMessage model.pressedKeys
                    }
            in
            ( nextModel, Cmd.none )



{- SUBSCRIPTIONS -}


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ Sub.map KeyMessage Keyboard.subscriptions
        ]
