port module Presentation exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Navigation
import Html exposing (Html)
import Html.Attributes as Attribute
import Http
import Keyboard exposing (Key(..))
import MicroKanren exposing (StreamModel, streamModelFromGoal)
import MicroKanren.Kernel exposing (..)
import Presentation.Debug exposing (viewKeys)
import Presentation.Goals exposing (goals)
import Presentation.Kernel exposing (..)
import Presentation.Parser as Parser exposing (Error(..))
import Task exposing (Task)
import Url exposing (Url)
import Url.Builder as UrlBuilder exposing (Root(..))


main : Program Flags Model Message
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        }


type alias Flags =
    { url : String
    }



{- MODEL -}


init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Message )
init flags origin key =
    let
        model =
            emptyPresentation
                |> createModel origin key
                |> updateStatus Loading

        command =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Accept" "text/markdown, text/plain" ]
                , url = flags.url
                , body = Http.emptyBody
                , expect = Http.expectString Got
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    ( model, command )


type alias Model =
    { base : Url
    , navigationKey : Navigation.Key
    , status : Status
    , pressedKeys : List Key
    , presentation : Presentation
    }


type Status
    = Idle
    | Loading
    | RequestSuccess
    | RequestFailure Http.Error
    | ParseFailure Parser.Error


createModel : Url -> Navigation.Key -> Presentation -> Model
createModel base navigationKey presentation =
    { base = base, navigationKey = navigationKey, status = Idle, pressedKeys = [], presentation = presentation }


updateStatus : Status -> Model -> Model
updateStatus status model =
    { model | status = status }



{- VIEW -}


view : Model -> Document Message
view model =
    { title = "μKanren"
    , body =
        [ Html.div [ Attribute.class "presentation" ]
            [ viewPresentation (\_ -> TakeFromStream) model.presentation
            , viewInfo model.presentation
            , viewStatus model.status
            ]
        ]
    }


viewStatus : Status -> Html msg
viewStatus status =
    let
        errorText =
            case status of
                RequestFailure error ->
                    Just "Request Failed"

                ParseFailure error ->
                    case error of
                        NoSlides ->
                            Just "No slides to parse"

                        NoGoalKnown label ->
                            Just <| "no goal for label '" ++ label ++ "'"

                _ ->
                    Nothing
    in
    case errorText of
        Just text ->
            Html.div
                [ Attribute.classList
                    [ ( "status", True )
                    , ( "error", True )
                    ]
                ]
                [ Html.span [] [ Html.text text ] ]

        Nothing ->
            Html.div [ Attribute.class "status" ] []



{- UPDATE -}


type Message
    = KeyMessage Keyboard.Msg
    | Advance
    | Backtrack
    | TakeFromStream
    | Got (Result Http.Error String)
    | Parse String
    | DoNothing


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Got result ->
            case result of
                Ok source ->
                    let
                        nextModel =
                            model
                                |> updateStatus RequestSuccess

                        task =
                            Task.succeed source
                    in
                    ( nextModel, Task.perform Parse task )

                Err error ->
                    let
                        nextModel =
                            model
                                |> updateStatus (RequestFailure error)
                    in
                    ( nextModel, Cmd.none )

        Parse source ->
            case Parser.parse goals source of
                Ok presentation ->
                    let
                        aPresentation =
                            model.base.fragment
                            |> Maybe.andThen String.toInt
                            |> Maybe.map (\targetIndex -> jump targetIndex presentation)
                            |> Maybe.withDefault presentation


                        nextModel =
                            createModel model.base model.navigationKey aPresentation
                                |> updateStatus Idle
                    in
                    ( nextModel, slideChanged ())

                Err error ->
                    let
                        nextModel =
                            model
                                |> updateStatus (ParseFailure error)
                    in
                    ( nextModel, Cmd.none )

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
                presentation =
                    advance model.presentation

                nextModel =
                    { model | presentation = presentation }

                index =
                    currentIndex presentation
                        |> String.fromInt

                url =
                    UrlBuilder.custom Relative [] [] (Just index)

                navigationCommand =
                    Navigation.pushUrl model.navigationKey url
            in
            ( nextModel, Cmd.batch [ slideChanged (), navigationCommand ] )

        Backtrack ->
            let
                presentation =
                    backtrack model.presentation

                nextModel =
                    { model | presentation = presentation }

                index =
                    currentIndex presentation
                        |> String.fromInt

                url =
                    UrlBuilder.custom Relative [] [] (Just index)

                navigationCommand =
                    Navigation.pushUrl model.navigationKey url
            in
            ( nextModel, Cmd.batch [ slideChanged (), navigationCommand ] )

        TakeFromStream ->
            let
                nextModel =
                    { model | presentation = takeFromStream model.presentation }
            in
            ( nextModel, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )


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


onUrlChange : Url -> Message
onUrlChange _ =
    DoNothing


onUrlRequest : UrlRequest -> Message
onUrlRequest _ =
    DoNothing



{- SUBSCRIPTIONS -}


port slideChanged : () -> Cmd msg


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ Sub.map KeyMessage Keyboard.subscriptions
        ]
