module Snake exposing
  ( tailToHead
  , update
  , growSnake
  , view
  , Snake
  )

import Color exposing (..)
import Collage exposing (..)
import Utils.Math exposing ( wrap )
import Utils.Color exposing ( ringColor )
import Tile exposing (tile, tiles, world)
import Position exposing
  ( Vector
  , vecEql
  , detectCollisions
  )

type alias Snake =
  { pos: Vector, body: List Vector }

type alias Dimensions =
  { width: Float, height: Float }

tailToHead : a -> List a -> List a
tailToHead leader body =
    List.append [leader] (List.take (List.length body - 1) body)

update : String -> Snake -> Snake
update direction snake =
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

growSnake : Snake -> Snake
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

ringView : Color -> Vector -> Form
ringView color ring  =
  Tile.view color ring

view : Snake -> List Form
view snake =
  let
    length = List.length snake.body
  in
    (List.indexedMap
      (\idx ring ->
        ringView (ringColor (length - idx) length) ring )
      snake.body)
