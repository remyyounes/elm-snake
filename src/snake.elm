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
    ChangeDirection direction ->
      (Model (updateGame msg model.game), Cmd.none)
    Tick dt ->
      (Model (updateGame msg model.game), Cmd.none)

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

view : Model -> Html Msg
view model =
  toHtml  <| color lightGray
          <| container 800 800 middle
          <| viewGame model.game
