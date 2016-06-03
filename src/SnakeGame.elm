module SnakeGame exposing (updateGame, initGame, viewGame, Game)
import SnakeMsg exposing (..)
import Time exposing (Time)
import SnakeLogic exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Random exposing (pair, int)
import Position exposing (..)
import TypeList exposing (..)
import Utils.Color exposing (..)
import Fruit
import Tile exposing (tiles, world)
-----------------
-- INIT VARIABLES
-- TODO: Move in GameState ?
-----------------
fps = 15
timePerFrame = 1000 / fps

type alias Game =
  { score: Int
  , previousDirection: String
  , direction: String
  , lastFrameDelta: Time
  , fruit: Fruit.Fruit
  , state: GameState
  , snake: Snake }

-------
-- Init
-------

initGame : Game
initGame =
  { score = 0
  , previousDirection = "Right"
  , direction = "Right"
  , lastFrameDelta = 0
  , fruit = (Fruit.init 6 0)
  , state = Playing
  , snake = initSnake }


maxBonus : Int
maxBonus = 100

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
      ( {game | fruit = Fruit.init x y }
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
    updatedFruit = Fruit.update fruit
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





-------
-- View
-------

renderRing: Color -> Vector -> Form
renderRing color ring  =
  Tile.view color ring

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
  <| List.append (renderSnake game.snake) [(Fruit.view game.fruit)]
