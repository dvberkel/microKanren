module Presentation exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attribute
import Http
import Keyboard exposing (Key(..))
import MicroKanren exposing (StreamModel, streamModelFromGoal)
import MicroKanren.Kernel exposing (..)
import Presentation.Debug exposing (viewKeys)
import Presentation.Kernel exposing (..)
import Presentation.Parser as Parser
import Task exposing (Task)


main : Program Flags Model Message
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Flags =
    { url : String
    }



{- MODEL -}


init : Flags -> ( Model, Cmd Message )
init flags =
    let
        model =
            emptyPresentation
                |> createModel
                |> updateStatus Loading

        command =
            Http.request
                { method = "GET"
                , headers = [ Http.header "Accept" "text/plain" ]
                , url = flags.url
                , body = Http.emptyBody
                , expect = Http.expectString Got
                , timeout = Nothing
                , tracker = Nothing
                }
    in
    ( model, command )


type alias Model =
    { status : Status
    , pressedKeys : List Key
    , presentation : Presentation
    }


type Status
    = Idle
    | Loading
    | RequestSuccess
    | RequestFailure Http.Error
    | ParseFailure Parser.Error


createModel : Presentation -> Model
createModel presentation =
    { status = Idle, pressedKeys = [], presentation = presentation }


updateStatus : Status -> Model -> Model
updateStatus status model =
    { model | status = status }



{- VIEW -}


view : Model -> Html Message
view model =
    Html.div [ Attribute.class "presentation" ]
        [ viewPresentation (\_ -> TakeFromStream) model.presentation
        , viewInfo model.presentation
        ]



{- UPDATE -}


type Message
    = KeyMessage Keyboard.Msg
    | Advance
    | Backtrack
    | TakeFromStream
    | Got (Result Http.Error String)
    | Parse String


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
            case Parser.parse source of
                Ok presentation ->
                    let
                        nextModel =
                            createModel presentation
                                |> updateStatus Idle
                    in
                    ( nextModel, Cmd.none )

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



{- SUBSCRIPTIONS -}


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ Sub.map KeyMessage Keyboard.subscriptions
        ]
