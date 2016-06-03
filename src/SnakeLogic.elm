module SnakeLogic exposing
  ( tailToHead
  , updateSnake
  , restrictDirection
  , growSnake
  )

import TypeList exposing (..)
import Utils.Math exposing ( wrap )
import Tile exposing (tile, tiles, world)
import Position exposing
  ( vecEql
  , detectCollisions
  )

tailToHead : a -> List a -> List a
tailToHead leader body =
    List.append [leader] (List.take (List.length body - 1) body)

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

restrictDirection previous next =
  case next of
    "Right" -> if previous == "Left" then previous else next
    "Left" -> if previous == "Right" then previous else next
    "Up" -> if previous == "Down" then previous else next
    "Down" -> if previous == "Up" then previous else next
    _ -> next
