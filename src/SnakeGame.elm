module SnakeGame exposing (updateGame, initGame, Game, viewGame)
import SnakeMsg exposing (..)
import Time exposing (Time)
import SnakeLogic exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)

-----------------
-- INIT VARIABLES
-- TODO: Move in GameState ?
-----------------
fps = 10
timePerFrame = 1000 / fps
tile = 20
tiles = 20
world =
  { width = tiles * tile, height = tiles * tile }

--------
-- Types
--------

type alias Vector =
  { x: Float, y: Float }

type alias Dimensions =
  { width: Float, height: Float }

type alias Snake =
  { pos: Vector, body: List Vector }

type alias Game =
  { score: Int, direction: String, lastFrameDelta: Time, snake: Snake }

type RingColor
  = Red
  | Blue
-------
-- Init
-------

initGame : Game
initGame =
  { score = 0, direction = "Right", lastFrameDelta = 0, snake = initSnake }

initVectorList : List Vector
initVectorList =
  List.map (\_ -> initVector) [1..10]

initVector : Vector
initVector =
  Vector (toFloat tiles / 2) (toFloat tiles / 2)

initSnake : Snake
initSnake =
  { pos = initVector, body = initVectorList}

---------
-- Update
---------

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

updateSnake : String -> Snake -> Snake
updateSnake direction snake = snake
  |> updateSnakePosition direction
  |> wrapSnakePosition world
  |> updateSnakeBody

updateSnakePosition : String -> Snake -> Snake
updateSnakePosition direction snake =
  { snake | pos = updatePosition direction snake.pos }

wrapSnakePosition : Dimensions -> Snake -> Snake
wrapSnakePosition bounds snake =
  { snake | pos = wrapPosition world snake.pos }

wrapPosition : Dimensions -> Vector -> Vector
wrapPosition world position =
  Vector
    (wrap position.x tiles)
    (wrap position.y tiles)

updateSnakeBody : Snake -> Snake
updateSnakeBody snake =
  { snake | body = tailToHead snake.pos snake.body }

updatePosition : String -> Vector -> Vector
updatePosition direction pos =
    case direction of
      "Right" -> Vector (pos.x + 1) pos.y
      "Left" -> Vector (pos.x - 1) pos.y
      "Up" -> Vector pos.x (pos.y + 1)
      "Down" -> Vector pos.x (pos.y - 1)
      _ -> pos

-------
-- View
-------

renderRing: Color -> Vector -> Form
renderRing color ring  =
  filled color (square tile) -- draw to tile dimensions
    |>  move (ring.x * tile, ring.y * tile) -- move according to tile size
    |>  move (tile/2, tile/2) -- displace origin to (0, 0)
    |>  move (-world.width/2, -world.height/2) -- mid to absolut coordinates

renderSnake : Snake -> List Form
renderSnake snake =
  let
    length = List.length snake.body
  in
    (List.indexedMap
      (\idx ring ->
        renderRing (getColor Red (length - idx) length) ring )
      snake.body)

viewGame : Game -> Element
viewGame game =
  color grey
  <| collage world.width world.height (renderSnake game.snake)

getColor: RingColor -> Int -> Int -> Color
getColor c idx length =
  let
    rank = toFloat idx / toFloat length
    val = round (lerp 120 200 rank)
    grey = val // 3
  in
    case c of
      Blue ->
        rgb grey grey val
      Red ->
        rgb val grey grey
