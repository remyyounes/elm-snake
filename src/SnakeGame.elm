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
fps = 15
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
  , state: GameState
  , snake: Snake }

type TileColor
  = Red
  | Blue

type GameState
  = Over
  | Playing
-------
-- Init
-------

initGame : Game
initGame =
  { score = 0
  , direction = "Right"
  , lastFrameDelta = 0
  , fruit = (newFruit 15 15)
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


initSnakeLength : Int
initSnakeLength = 3

initPos length pos =
  Vector
    (toFloat (length - pos))
    0.0

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

vecEql vecA vecB =
  vecA.x == vecB.x &&
  vecA.y == vecB.y


updateGame : Msg -> Game -> ( Game, Cmd Msg )
updateGame msg game =
  case msg of
    ChangeDirection direction ->
      ({game | direction = direction}, Cmd.none)
    NewFruit ( x, y ) ->
      ( {game | fruit = newFruit x y }
      , Cmd.none)
    Tick dt ->
      let
        a = Debug.log ">>>>" snake.pos
        newDelta = game.lastFrameDelta + dt
        { score, snake, lastFrameDelta, direction, fruit } = game
        ateFruit = vecEql snake.pos fruit.pos
        grownSnake = if ateFruit then growSnake snake else snake
        updatedSnake = (updateSnake direction grownSnake)
        ateTail = detectCollisions updatedSnake.body
        updatedFruit = updateFruit fruit
        gameState = if ateTail || game.state == Over then Over else Playing
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
          if newDelta > timePerFrame && game.state == Playing then
            { game
            | lastFrameDelta = lastFrameDelta - timePerFrame
            , score = score + round newDelta
            , fruit = updatedFruit
            , state = gameState
            , snake = updatedSnake}
          else
            { game
            | lastFrameDelta = newDelta
            , state = Debug.log " > " gameState
            , snake = grownSnake}
      in
        (g, cmds)
    _ ->
      (game, Cmd.none)

detectCollisions body =
  let
    nextTail = List.tail body
  in
  case nextTail of
    Just tail ->
      case List.head body of
        Just head -> detectCollision head tail
        Nothing -> False
    Nothing -> False

detectCollision head tail =
  let
    collided =
      List.foldl
        (\tile m -> comp head tile)
        False
        tail
  in
    if collided then True else detectCollisions tail

comp head tail =
  tail.x == head.x && tail.y == head.y

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
        renderRing (ringColor Red (length - idx) length) ring )
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
      Blue -> rgb grey grey val
      Red -> rgb val grey grey

ringColor: TileColor -> Int -> Int -> Color
ringColor color idx length =
  let
    rank = toFloat idx / toFloat length
    val = round (lerp 120 200 rank)
    grey = val // 3
  in
    case color of
      Blue -> rgb grey grey val
      Red -> rgb val grey grey
