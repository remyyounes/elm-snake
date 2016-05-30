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

fps = 10
timePerFrame = 1000 / fps
tile = 20
tiles = 20
world = { width = tiles * tile, height = tiles * tile }
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

type alias Vector =
   { x: Float, y: Float }

type alias Snake =
   { pos: Vector, body: List Vector }

type alias Game =
   { score: Int, direction: String, lastFrameDelta: Time, snake: Snake }

type alias Model =
   { game: Game }

initGame =
   { score = 0, direction = "Right", lastFrameDelta = 0, snake = initSnake }

initVectorList : List Vector
initVectorList = List.map (\_ -> initVector) [1..10]

initVector : Vector
initVector = Vector (toFloat tiles / 2) (toFloat tiles / 2)

initSnake = { pos = initVector, body = initVectorList}

init : ( Model, Cmd Msg )
init = (Model initGame, Cmd.none)

---------
-- UPDATE
---------

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
      (Model (updateGame msg model.game), Cmd.none)
    Tick dt ->
      (Model (updateGame msg model.game), Cmd.none)


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
          , snake = (updateSnake direction snake)
          }
        else
          {game | lastFrameDelta = newDelta}
    _ ->
      game

updateSnake direction snake = snake
  |> updateSnakePosition direction
  |> wrapSnakePosition world
  |> updateSnakeBody

updateSnakePosition direction snake =
  { snake | pos = updatePosition direction snake.pos }

wrapSnakePosition bounds snake =
  { snake | pos = wrapPosition world snake.pos }

wrapPosition world position =
  Vector
    (wrap position.x tiles)
    (wrap position.y tiles)

wrap v range =
  let
    min = 0
    max = range - 1
  in
    if v < min then
      max
    else if v > max then
      min
    else
      v

updateSnakeBody snake =
  { snake | body = followLeader snake.pos snake.body }

followLeader leader body =
    List.append [leader] (List.take (List.length body - 1) body)

updatePosition direction pos =
    case direction of
      "Right" -> Vector (pos.x + 1) pos.y
      "Left" -> Vector (pos.x - 1) pos.y
      "Up" -> Vector pos.x (pos.y + 1)
      "Down" -> Vector pos.x (pos.y - 1)
      _ -> pos


-------
-- SUBS
-------

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    kb = Keyboard.downs keyboardProcessor
    tick = AnimationFrame.diffs Tick
  in
    Sub.batch [kb, tick]

-------
-- VIEW
-------

(=>) = (,)

renderRing ring =
  filled lightCharcoal (square tile) -- draw to tile dimensions
    |>  move (ring.x * tile, ring.y * tile) -- move according to tile size
    |>  move (-world.width/2, -world.height/2) -- mid to absolut coordinates
    |>  move (tile/2, tile/2) -- displace origin to (0, 0)

renderSnake snake =
  List.map renderRing snake.body

view : Model -> Html Msg
view model =
  toHtml  <| color lightGray
          <| container 800 800 middle
          <| color grey
          <| collage world.width world.height (renderSnake model.game.snake)

--------------
-- KEY HELPERS
--------------

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
