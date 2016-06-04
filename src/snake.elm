module Snake exposing
  ( tailToHead
  , update
  , growSnake
  , view
  , Snake
  , Ring
  )

import Debug

import SnakeMsg exposing (..)
import Color exposing (..)
import Collage exposing (..)
import Utils.Math exposing ( wrap )
import Utils.Color exposing ( ringColor )
import Tile exposing (tile, tiles, world)
import Position exposing
  ( Vector
  , vecEql
  , detectCollisions
  , wrapPosition
  , Dimensions
  )

type alias Ring =
  { pos: Vector
  , orientation: String
  }

type alias Snake =
  { pos: Vector
  , body: List Ring
  , direction: String
  , previousDirection: String
  }

tailToHead : a -> List a -> List a
tailToHead leader body =
  List.append [leader] (List.take (List.length body - 1) body)

update : Msg -> Snake -> Snake
update msg snake =
  case msg of
    Tick dt ->
      snake
        |> updateSnakePosition snake.direction
        |> wrapSnakePosition world
        |> updateSnakeBody
        |> rememberDirection
    ChangeDirection direction ->
      { snake | direction =
        restrictDirection snake.previousDirection direction }
    _ -> snake

rememberDirection snake =
  { snake | previousDirection = snake.direction }

updateSnakePosition : String -> Snake -> Snake
updateSnakePosition direction snake =
  { snake | pos = updatePosition direction snake.pos }

wrapSnakePosition : Dimensions -> Snake -> Snake
wrapSnakePosition bounds snake =
  { snake | pos = wrapPosition world snake.pos }


growSnake : Snake -> Snake
growSnake snake =
  let
    length = (List.length snake.body) - 1
  in
    { snake
    | body = List.append snake.body (List.drop length snake.body)}

updateSnakeBody : Snake -> Snake
updateSnakeBody snake =
  let
    orientationCur = snake.direction ++ snake.direction
    orientationPrev = snake.previousDirection ++ snake.direction
    head = { pos = snake.pos, orientation = orientationCur }
    prevBody = List.append
      (List.map (setOrientation orientationPrev) (List.take 1 snake.body))
      (List.drop 1 snake.body)
  in
    { snake | body = tailToHead head prevBody }


setOrientation orientation ring =
  { ring | orientation = orientation}

updatePosition : String -> Vector -> Vector
updatePosition direction pos =
  case direction of
    "Right" -> Vector (pos.x + 1) pos.y
    "Left" -> Vector (pos.x - 1) pos.y
    "Up" -> Vector pos.x (pos.y + 1)
    "Down" -> Vector pos.x (pos.y - 1)
    _ -> pos


restrictDirection : String -> String -> String
restrictDirection previous next =
  case next of
    "Right" -> if previous == "Left" then previous else next
    "Left" -> if previous == "Right" then previous else next
    "Up" -> if previous == "Down" then previous else next
    "Down" -> if previous == "Up" then previous else next
    _ -> next

-------
-- View
-------

view : Snake -> List Form
view snake =
  let
    length = List.length snake.body
  in
    (List.indexedMap
      (\idx ring ->
        let
          color = (ringColor (length - idx) length)
        in
          if idx == 0 then
            Tile.viewHead green ring.pos ring.orientation
          else if idx == length - 1 then
            Tile.viewTail color ring.pos ring.orientation
          else
            Tile.view color ring.pos
      )
      snake.body
    )
