module Presentation exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attribute
import Keyboard exposing (Key(..))
import MicroKanren exposing (StreamModel, streamModelFromGoal)
import MicroKanren.Kernel exposing (..)
import Presentation.Debug exposing (viewKeys)
import Presentation.Kernel exposing (..)
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
        model =
            emptyPresentation
            |> createModel
            |> updateFetchStatus Loading
    in
    ( model, Cmd.none )


type alias Model =
    { fetchStatus : FetchStatus
    , pressedKeys : List Key
    , presentation : Presentation
    }


type FetchStatus
    = Idle
    | Loading
    | Success
    | Failure


createModel : Presentation -> Model
createModel presentation =
    { fetchStatus = Idle, pressedKeys = [], presentation = presentation }


updateFetchStatus : FetchStatus -> Model -> Model
updateFetchStatus fetchStatus model =
    { model | fetchStatus = Idle }



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



{- SUBSCRIPTIONS -}


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.batch
        [ Sub.map KeyMessage Keyboard.subscriptions
        ]
