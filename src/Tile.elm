module Tile exposing (tile, tiles, world, view, viewHead, viewTail, viewFruit)
import Collage exposing (..)
import Color exposing (..)

tile = 20
tiles = 20

world =
  { width = tiles * tile, height = tiles * tile }

view : Color -> {a | x : Float, y : Float} -> String -> Form
view color position orientation =
  (getShape color orientation)
    |> placeTile position

viewFruit color position orientation =
  (filled color (circle (tile/4)))
    |> placeTile position

viewTail color position direction =
  filled color (polygon [(-tile / 2, tile / 2), (tile / 2, tile / 2), (0, -tile / 2)] )
    |> rotate (degrees (getOrientation direction))
    |> placeTile position

viewHead color position direction =
  (group
    [ filled color (circle (tile / 2) )
    , filled color (rect tile (tile/2) ) |>  move (0, -tile / 4)
    , filled black (circle (tile/10) ) |>  move (-tile / 4, 0)
    , filled black (circle (tile/10) ) |>  move (tile / 4, 0)
    ]
  )
    |> rotate (degrees (getOrientation direction))
    |> placeTile position

placeTile position form =
  form
    |>  move (position.x * tile, position.y * tile)
    |>  move (tile / 2, tile / 2)
    |>  move (-world.width / 2, -world.height / 2)

getShape color orientation =
  let
    angle = degrees (cornerOrientation orientation)
    shape =
      case orientation of
        "UpRight" -> corner color tile
        "UpLeft" -> corner color tile
        "LeftUp" -> corner color tile
        "LeftDown" -> corner color tile
        "RightUp" -> corner color tile
        "RightDown" -> corner color tile
        "DownLeft" -> corner color tile
        "DownRight" -> corner color tile
        _ -> filled color (square tile)
  in
    shape |> rotate angle

corner color size =
  group
    [ filled color (circle (size / 2) )
    , filled color (rect size (size/2) ) |>  move (0, size / 4)
    , filled color (rect (size/2) size ) |>  move (size / 4, 0)
    ]


getOrientation orientation =
  case orientation of
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


cornerOrientation orientation =
  case orientation of
    "UpRight" -> 270.0
    "LeftDown" -> 270.0
    "DownRight" -> 0.0
    "LeftUp" -> 0.0
    "DownLeft" -> 90.0
    "RightUp" -> 90.0
    "UpLeft" -> 180.0
    "RightDown" -> 180.0
    _ -> 0.0
