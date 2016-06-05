import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))

import Color exposing (..)
import Element exposing (..)
import Collage exposing (..)

import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)

import SnakeMsg exposing (..)
import SnakeKeyboard exposing (keyboardProcessor)
import SnakeGame exposing (..)

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

--------
-- MODEL
--------

type alias Model = { game: Game }

init : ( Model, Cmd Msg )
init = (Model initGame, Cmd.none)

---------
-- UPDATE
---------

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    Start ->
      init
    NewFruit pos ->
      let
        (game, cmds) = (updateGame msg model.game)
      in
        (Model game, cmds)
    ChangeDirection direction ->
      let
        (game, cmds) = (updateGame msg model.game)
      in
        (Model game, cmds)
    Tick dt ->
      let
        (game, cmds) = (updateGame msg model.game)
      in
        (Model game, cmds)

-------
-- SUBS
-------

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs keyboardProcessor
    , AnimationFrame.diffs Tick ]

-------
-- VIEW
-------

(=>) = (,)

containerStyle =
  style
    [ ("backgroundColor", "rgb(238, 238, 236)")
    , ("width", "100%")
    , ("height", "100%")
    , ("display", "flex")
    , ("align-items", "center")
    , ("justify-content", "center")
    ]

instructionsStyle =
  style
    [ ("color", "gray")
    , ("font-family", "monospace")
    , ("padding", "5px")]

instructions =
  Html.text "A,W,S,D / ⇨, ⇩, ⇦, ⇧  --  SPACE"

view : Model -> Html Msg
view model =
  div [containerStyle]
      [ div []
            [ (toHtml <| viewGame model.game)
            , div [instructionsStyle] [instructions]
            ]
      ]
