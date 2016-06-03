module SnakeGame exposing (updateGame, initGame, viewGame)
import SnakeMsg exposing (..)
import Time exposing (Time)
import SnakeLogic exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Random exposing (pair, int)
import Position exposing (..)
import TypeList exposing (..)
-----------------
-- INIT VARIABLES
-- TODO: Move in GameState ?
-----------------
fps = 15
timePerFrame = 1000 / fps

-------
-- Init
-------

initGame : Game
initGame =
  { score = 0
  , previousDirection = "Right"
  , direction = "Right"
  , lastFrameDelta = 0
  , fruit = (newFruit 6 0)
  , state = Playing
  , snake = initSnake }

initVector : Vector
initVector =
  Vector (toFloat tiles / 2) (toFloat tiles / 2)

maxBonus : Int
maxBonus = 100

newFruit : Int -> Int -> Fruit
newFruit x y =
  { pos = Vector (toFloat x) (toFloat y)
  , bonus = maxBonus }

initPos length pos =
  Vector
    (toFloat (length - pos))
    0.0

initSnakeLength : Int
initSnakeLength = 3

initSnake : Snake
initSnake =
  let
  offset = initSnakeLength // 2
  in
    { pos = initPos initSnakeLength 1
    , body = List.map (initPos initSnakeLength) [1..initSnakeLength] }

---------
-- Update
---------

updateGame : Msg -> Game -> ( Game, Cmd Msg )
updateGame msg game =
  case msg of
    ChangeDirection direction ->
      ({game | direction =
        restrictDirection game.previousDirection direction}
      , Cmd.none)
    NewFruit ( x, y ) ->
      ( {game | fruit = newFruit x y }
      , Cmd.none)
    Tick dt ->
      let
        newDelta = game.lastFrameDelta + dt
        ticked = newDelta > timePerFrame && game.state == Playing
        newFrameDelta =
          case ticked of
            True -> game.lastFrameDelta - timePerFrame
            False -> newDelta
        g = { game | lastFrameDelta = newFrameDelta }
      in
        if ticked then
          stepGame g
        else
          ( g , Cmd.none )
    _ ->
      (game, Cmd.none)


stepGame game =
  let
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
    updatedSnake = (updateSnake direction grownSnake)
    ateTail = detectCollisions updatedSnake.body
    updatedFruit = updateFruit fruit
    gameState = if ateTail || game.state == Over then Over else Playing
  in
    (
      { game
      | score = score
      , fruit = updatedFruit
      , state = gameState
      , previousDirection = direction
      , snake = updatedSnake}
    , cmds)


updateFruit fruit =
  { fruit | bonus = fruit.bonus - 1 }


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
  renderTile (fruitColor Red fruit) fruit.pos

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
        renderRing (ringColor Green (length - idx) length) ring )
      snake.body)

viewGame : Game -> Element
viewGame game =
  color grey
  <| collage world.width world.height
  <| List.append (renderSnake game.snake) [(renderFruit game.fruit)]

fruitColor: TileColor -> Fruit -> Color
fruitColor color fruit=
  let
    rank = toFloat fruit.bonus / toFloat maxBonus
    val = round (lerp 0 200 rank)
    grey = val // 3
  in
    case color of
      Red -> rgb val grey grey
      Green -> rgb grey val grey
      Blue -> rgb grey grey val

ringColor: TileColor -> Int -> Int -> Color
ringColor color idx length =
  let
    rank = toFloat idx / toFloat length
    val = round (lerp 100 200 rank)
    grey = val // 3
  in
    case color of
      Red -> rgb val grey grey
      Green -> rgb grey val grey
      Blue -> rgb grey grey val
