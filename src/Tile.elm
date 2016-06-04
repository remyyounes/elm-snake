module Tile exposing (tile, tiles, world, view, viewHead, viewTail)
import Collage exposing (..)
import Color exposing (..)

tile = 20
tiles = 20

world =
  { width = tiles * tile, height = tiles * tile }

view : Color -> {a | x : Float, y : Float} -> Form
view color position =
  filled color (square tile)
    |> placeTile position

viewTail color position direction =
  filled color (polygon [(-tile / 2, tile / 2), (tile / 2, tile / 2), (0, -tile / 2)] )
    |> rotate (degrees (directionAngle direction))
    |> placeTile position

viewHead color position direction =
  (group
    [ filled color (circle (tile / 2) )
    , filled color (rect tile (tile/2) ) |>  move (0, -tile / 4)
    , filled black (circle (tile/10) ) |>  move (-tile / 4, 0)
    , filled black (circle (tile/10) ) |>  move (tile / 4, 0)
    ]
  )
    |> rotate (degrees (directionAngle direction))
    |> placeTile position

placeTile position form =
  form
    |>  move (position.x * tile, position.y * tile)
    |>  move (tile / 2, tile / 2)
    |>  move (-world.width / 2, -world.height / 2)

directionAngle direction =
  case direction of
    "UpUp" -> 0.0
    "UpRight" -> 270.0
    "UpLeft" -> 90.0

    "DownDown" -> 180.0
    "DownRight" -> 270.0
    "DownLeft" -> 90.0

    "RightRight" -> 270.0
    "RightUp" -> 0.0
    "RightDown" -> 180.0

    "LeftLeft" -> 90.0
    "LeftUp" -> 0.0
    "LeftDown" -> 180.0

    _ -> 0.0
