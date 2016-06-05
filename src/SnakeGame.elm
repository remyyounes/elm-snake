module SnakeGame exposing (updateGame, initGame, viewGame, Game)
import SnakeMsg exposing (..)
import Time exposing (Time)
import Snake
import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Random exposing (pair, int)
import Position exposing (..)
import Utils.Color exposing (..)
import Fruit
import Tile exposing (tiles, world)

-----------------
-- INIT VARIABLES
-- TODO: Move in GameState ?
-----------------
fps = 10
timePerFrame = 1000 / fps

type GameState
  = Over
  | Playing

type alias Game =
  { score: Int
  , previousDirection: String
  , direction: String
  , lastFrameDelta: Time
  , fruit: Fruit.Fruit
  , state: GameState
  , snake: Snake.Snake }

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

-- "⎡"
-- "⎣"
-- "⎤"
-- "⎦"
-- "_"
-- "|"
initPos : Int -> Int -> Vector
initPos length pos =
  Vector (toFloat (length - pos))  0.0

initSnakeLength : Int
initSnakeLength = 3

initRing : Int -> Int -> Snake.Ring
initRing length pos =
  { orientation = "RightRight"
  , pos = initPos length pos }

initSnake : Snake.Snake
initSnake =
  let
    offset = initSnakeLength // 2
  in
    { pos = initPos initSnakeLength 1
    , body = List.map (initRing initSnakeLength) [1..initSnakeLength]
    , direction = "Right"
    , previousDirection = "Right"
    }

---------
-- Update
---------

updateGame : Msg -> Game -> ( Game, Cmd Msg )
updateGame msg game =
  case msg of
    ChangeDirection direction ->
      stepGame msg game
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
          stepGame msg g
        else
          ( g , Cmd.none )
    _ ->
      (game, Cmd.none)


stepGame : Msg -> Game -> ( Game, Cmd Msg )
stepGame msg game =
  case msg of
    ChangeDirection direction ->
      ({game | snake = Snake.update msg game.snake}
      , Cmd.none)
    Tick dt ->
      let
        { score, snake, lastFrameDelta, direction, fruit } = game
        ateFruit = vecEql snake.pos fruit.pos
        grownSnake = if ateFruit then Snake.growSnake snake else snake
        cmds =
          if ateFruit then
            Random.generate
              NewFruit
              (pair
                (int 0 (tiles - 1))
                (int 0 (tiles - 1)))
          else
            Cmd.none
        updatedSnake = (Snake.update msg grownSnake)
        ateTail = detectCollisions (List.map .pos updatedSnake.body)
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
    _ -> (game, Cmd.none)

-------
-- View
-------

viewGame : Game -> Element
viewGame game =
  color grey
  <| collage world.width world.height
  <| List.append [(Fruit.view game.fruit)] (Snake.view game.snake)
