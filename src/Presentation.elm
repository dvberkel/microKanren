module Presentation exposing (Presentation, Slide(..))

import Browser
import Html exposing (Html)
import Html.Attributes as Attribute


main : Program () Model msg
main =
    Browser.element
        { init = \_ -> init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



{- MODEL -}


init : ( Model, Cmd msg )
init =
    let
        presentation =
            [ Blank, Blank, Blank ]
                |> fromList
                |> Maybe.withDefault emptyPresentation
    in
    ( presentation, Cmd.none )


type alias Model =
    Presentation


type Presentation
    = Presentation
        { preceding : List Slide
        , current : Slide
        , following : List Slide
        }


type Slide
    = Blank


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


slideCount : Presentation -> Int
slideCount (Presentation { preceding, current, following }) =
    List.length preceding + 1 + List.length following



{- VIEW -}


view : Model -> Html msg
view model =
    Html.div [ Attribute.class "presentation" ]
        [ viewInfo model
        ]


viewInfo : Model -> Html msg
viewInfo model =
    Html.div [ Attribute.class "info" ]
        [ viewCount model
        ]


viewCount : Model -> Html msg
viewCount model =
    Html.div [ Attribute.class "count" ]
        [ Html.span [ Attribute.class "total" ] [ Html.text <| String.fromInt <| slideCount model ]
        ]



{- UPDATE -}


update : msg -> Model -> ( Model, Cmd msg )
update _ model =
    ( model, Cmd.none )



{- SUBSCRIPTIONS -}


subscriptions : Model -> Sub msg
subscriptions model =
    Sub.none
