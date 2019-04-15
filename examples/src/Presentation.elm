module Presentation exposing (Presentation, Slide(..))

import Browser
import Html exposing (Html)
import Html.Attributes as Attribute
import Keyboard exposing (Key(..))
import Markdown as TransformMarkdown
import MicroKanren exposing (StreamModel, streamModelFromGoal)
import MicroKanren.Kernel exposing (..)
import Presentation.Debug exposing (viewKeys)
import Task exposing (Task)


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
            [ Blank
            , Markdown """
# Presentation
## with a sub-title
"""
            , Stream <| streamModelFromGoal "≡ t 5" <| callFresh (\term -> identical term (Value 5))
            ]
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
    | Markdown String
    | Stream (StreamModel Int)


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
        [ viewSlides model.presentation
        , viewInfo model.presentation
        ]


viewSlides : Presentation -> Html Message
viewSlides (Presentation data) =
    viewSlide data.current


viewSlide : Slide -> Html Message
viewSlide slide =
    let
        content =
            case slide of
                Blank ->
                    []

                Markdown source ->
                    [ TransformMarkdown.toHtml [ Attribute.class "content" ] source
                    ]

                Stream streamModel ->
                    let
                        streamModelView =
                            streamModel
                                |> MicroKanren.view String.fromInt
                                |> Html.map (\_ -> TakeFromStream)
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
            "stream"


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
    | Advance
    | Backtrack
    | TakeFromStream


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        KeyMessage keyMessage ->
            let
                pressedKeys =
                    Keyboard.update keyMessage model.pressedKeys

                nextModel =
                    { model
                        | pressedKeys = pressedKeys
                    }

                nextCommand =
                    pressedKeys
                        |> toCommand
                        |> Maybe.map (Task.perform identity)
                        |> Maybe.withDefault Cmd.none
            in
            ( nextModel, nextCommand )

        Advance ->
            let
                nextModel =
                    { model | presentation = advance model.presentation }
            in
            ( nextModel, Cmd.none )

        Backtrack ->
            let
                nextModel =
                    { model | presentation = backtrack model.presentation }
            in
            ( nextModel, Cmd.none )

        TakeFromStream ->
            let
                nextModel =
                    { model | presentation = takeFromStream model.presentation }
            in
            ( nextModel, Cmd.none )


toCommand : List Key -> Maybe (Task Never Message)
toCommand keys =
    case keys of
        [] ->
            Nothing

        ArrowRight :: _ ->
            Just <| Task.succeed Advance

        ArrowLeft :: _ ->
            Just <| Task.succeed Backtrack

        _ :: tail ->
            toCommand tail


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



{- SUBSCRIPTIONS -}


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ Sub.map KeyMessage Keyboard.subscriptions
        ]
