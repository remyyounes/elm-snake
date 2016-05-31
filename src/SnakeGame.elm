module SnakeGame exposing (updateGame, initGame, Game, viewGame)
import SnakeMsg exposing (..)
import Time exposing (Time)
import SnakeLogic exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Random exposing (pair, int)
-----------------
-- INIT VARIABLES
-- TODO: Move in GameState ?
-----------------
fps = 20
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

type alias Fruit =
  { pos: Vector, bonus: Int }

type alias Game =
  { score: Int
  , direction: String
  , lastFrameDelta: Time
  , fruit: Fruit
  , snake: Snake }

type RingColor
  = Red
  | Blue
-------
-- Init
-------

initGame : Game
initGame =
  { score = 0
  , direction = "Right"
  , lastFrameDelta = 0
  , fruit = initFruit
  , snake = initSnake }

initVectorList : List Vector
initVectorList =
  List.map (\_ -> initVector) [1..2]

initVector : Vector
initVector =
  Vector (toFloat tiles / 2) (toFloat tiles / 2)

initFruit : Fruit
initFruit =
  { pos = Vector 15 10, bonus = 20 }

initSnake : Snake
initSnake =
  { pos = initVector, body = initVectorList}

---------
-- Update
---------

vecEql vecA vecB =
  vecA.x == vecB.x &&
  vecA.y == vecB.y


updateGame : Msg -> Game -> ( Game, Cmd Msg )
updateGame msg game =
  case msg of
    ChangeDirection direction ->
      ({game | direction = direction}, Cmd.none)
    NewFruit ( x, y ) ->
      ({game | fruit = { pos = (Vector (toFloat x) (toFloat y)), bonus = 5}}, Cmd.none)
    Tick dt ->
      let
        newDelta = game.lastFrameDelta + dt
        { score, snake, lastFrameDelta, direction, fruit } = game
        ateFruit = vecEql snake.pos fruit.pos
        grownSnake = if ateFruit then growSnake snake else snake
        cmds =
          if ateFruit then
            Random.generate
              NewFruit
              (pair
                (int 0 (tiles - 1))
                (int 0 (tiles - 1)))
          else
            Cmd.none
        g =
          if  newDelta > timePerFrame then
            { game
            | lastFrameDelta = lastFrameDelta - timePerFrame
            , score = score + round newDelta
            , fruit = updateFruit fruit
            , snake = (updateSnake direction grownSnake)}
          else
            { game
            | lastFrameDelta = newDelta
            , snake = grownSnake}
      in
        (g, cmds)
    _ ->
      (game, Cmd.none)

updateFruit fruit =
  { fruit | bonus = fruit.bonus - 1 }


updateSnake : String -> Snake -> Snake
updateSnake direction snake =
  snake
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

growSnake snake =
  let
    length = (List.length snake.body) - 1
  in
    { snake
    | body = List.append snake.body (List.drop length snake.body)}

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

renderTile : Color -> Vector -> Form
renderTile color position =
  filled color (square tile) -- draw to tile dimensions
    |>  move (position.x * tile, position.y * tile) -- move according to tile size
    |>  move (tile/2, tile/2) -- displace origin to (0, 0)
    |>  move (-world.width/2, -world.height/2) -- mid to absolut coordinates

renderFruit: Fruit -> Form
renderFruit fruit =
  renderTile orange fruit.pos

renderRing: Color -> Vector -> Form
renderRing color ring  =
  renderTile color ring

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
  <| collage world.width world.height
  <| List.append (renderSnake game.snake) [(renderFruit game.fruit)]

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
