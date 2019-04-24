module Presentation.Kernel exposing (Presentation(..), Slide(..), advance, backtrack, currentIndex, emptyPresentation, fromList, slideCount, takeFromStream, toClassName, viewCount, viewInfo, viewPresentation, viewSlide)

import Html exposing (Html)
import Html.Attributes as Attribute
import Markdown as TransformMarkdown
import MicroKanren exposing (Message, StreamModel)


type Presentation
    = Presentation
        { preceding : List Slide
        , current : Slide
        , following : List Slide
        }


type Slide
    = Blank
    | Markdown String
    | Stream (StreamModel Int)


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


takeFromStream : Presentation -> Presentation
takeFromStream ((Presentation data) as original) =
    case data.current of
        Stream streamModel ->
            let
                nextStreamModel =
                    MicroKanren.update MicroKanren.TakeFromStream streamModel

                current =
                    Stream nextStreamModel
            in
            Presentation { data | current = current }

        _ ->
            original


viewPresentation : (Message -> msg) -> Presentation -> Html msg
viewPresentation messageMap ((Presentation data) as presentation) =
    viewSlide messageMap (currentIndex presentation) data.current


viewSlide : (Message -> msg) -> Int -> Slide -> Html msg
viewSlide messageMap index slide =
    let
        content =
            case slide of
                Blank ->
                    []

                Markdown source ->
                    [ TransformMarkdown.toHtml
                        [ Attribute.classList
                            [ ( "content", True )
                            , ( "first", index == 1 )
                            ]
                        ]
                        source
                    ]

                Stream streamModel ->
                    let
                        streamModelView =
                            streamModel
                                |> MicroKanren.view String.fromInt
                                |> Html.map messageMap
                    in
                    [ streamModelView
                    ]
    in
    Html.div
        [ Attribute.classList
            [ ( "slide", True )
            , ( toClassName slide, True )
            ]
        ]
        content


toClassName : Slide -> String
toClassName slide =
    case slide of
        Blank ->
            "blank"

        Markdown _ ->
            "markdown"

        Stream _ ->
            "streammodel"


viewInfo : Presentation -> Html msg
viewInfo presentation =
    Html.div [ Attribute.class "info" ]
        [ viewCount presentation
        ]


viewCount : Presentation -> Html msg
viewCount presentation =
    Html.div [ Attribute.class "count" ]
        [ Html.span [ Attribute.class "index" ] [ Html.text <| String.fromInt <| currentIndex presentation ]
        , Html.span [ Attribute.class "total" ] [ Html.text <| String.fromInt <| slideCount presentation ]
        ]
