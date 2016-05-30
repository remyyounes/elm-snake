import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Json.Decode as Json exposing ((:=))
import Keyboard exposing (KeyCode)
import AnimationFrame
import Time exposing (Time)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)


fps = 30
timePerFrame = 1000 / fps
tile = 20

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Vector =
  { x: Float, y: Float }

type alias Game =
  { score: Int, direction: String, lastFrameDelta: Time, snake: Snake }

type alias Snake =
  { head: Vector, body: Vector}

type alias Model =
  { game: Game }

initSnake =
  { head = Vector 0 0, body = Vector 0 0}

init : ( Model, Cmd Msg )
init = (Model (Game 0 "Right" 0 initSnake), Cmd.none)

type Msg
    = Start
    | NoOp
    | ChangeDirection String
    | Tick Time

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)
    Start ->
      init
    ChangeDirection direction ->
      (Model (updateGame msg model.game) , Cmd.none)
    Tick dt ->
      (Model (updateGame msg model.game) , Cmd.none)

-- HELPERS

updateGame : Msg -> Game -> Game
updateGame msg game =
  case msg of
    ChangeDirection direction ->
      {game | direction = direction}
    Tick dt ->
      let
        newDelta = game.lastFrameDelta + dt
        { score, snake, lastFrameDelta, direction } = game
      in
        if  newDelta > timePerFrame then
          { game
          | lastFrameDelta = lastFrameDelta - timePerFrame
          , score = score + round newDelta
          , snake = updateSnake direction snake
          }
        else
          {game | lastFrameDelta = newDelta}
    _ ->
      game

updateSnake direction snake =
  let
    head = snake.head
  in
    case direction of
      "Right" -> { snake | head = Vector (head.x + tile) head.y}
      "Left" -> { snake | head = Vector (head.x - tile) head.y}
      "Up" -> { snake | head = Vector head.x (head.y + tile)}
      "Down" -> { snake | head = Vector head.x (head.y - tile)}
      _ -> snake


subscriptions : Model -> Sub Msg
subscriptions model =
  let
    kb = Keyboard.downs keyboardProcessor
    tick = AnimationFrame.diffs Tick
  in
    Sub.batch [kb, tick]

-- VIEW

(=>) = (,)

renderSnake snake =
  let
    pos = snake.head
  in
    filled lightCharcoal (square tile) |>  move (pos.x, pos.y)

-- view : Model -> Html Msg
view model =
    toHtml  <| color lightGray <| container 800 800 middle
            <| color grey <| collage 400 400 [ renderSnake model.game.snake ]

  -- let
      -- score = toString model.game.score
      -- direction = text model.game.direction
      -- head = text (toString model.game.snake.head)
  -- in
    --
    -- div [ ]
    --   [ text score
    --   , direction
    --   , head
    --   ]

-- HELPERS


type Key
    = Space
    | ArrowLeft
    | ArrowRight
    | ArrowDown
    | ArrowUp
    | Unknown


fromCode : Int -> Key
fromCode keyCode =
  case keyCode of
    32 -> Space
    -- arrows
    38 -> ArrowUp
    39 -> ArrowRight
    40 -> ArrowDown
    37 -> ArrowLeft
    -- aswd
    87 -> ArrowUp
    68 -> ArrowRight
    83 -> ArrowDown
    65 -> ArrowLeft
    _ -> Unknown

keyboardProcessor keyCode =
  case fromCode keyCode of
    Space -> Start
    ArrowDown -> ChangeDirection "Down"
    ArrowLeft -> ChangeDirection "Left"
    ArrowRight -> ChangeDirection "Right"
    ArrowUp -> ChangeDirection "Up"
    _ -> NoOp
