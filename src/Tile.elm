module Tile exposing (tile, tiles, world, view, viewHead, viewTail, viewFruit)
import Collage exposing (..)
import Color exposing (..)

tile = 30
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
  filled color (triangle tile)
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
    cAngle = degrees (cornerOrientation orientation)
    tAngle = degrees (getOrientation orientation)
    shape =
      case orientation of
        "UpRight" -> corner color tile |> rotate cAngle
        "UpLeft" -> corner color tile |> rotate cAngle
        "LeftUp" -> corner color tile |> rotate cAngle
        "LeftDown" -> corner color tile |> rotate cAngle
        "RightUp" -> corner color tile |> rotate cAngle
        "RightDown" -> corner color tile |> rotate cAngle
        "DownLeft" -> corner color tile |> rotate cAngle
        "DownRight" -> corner color tile |> rotate cAngle
        _ -> patternTile color tile |> rotate tAngle
  in
    shape

triangle size =
  polygon
    [ (-size / 2, size / 2)
    , (size / 2, size / 2)
    , (0, -size / 2)
    ]

patternTile color size =
   group
     [ filled color (square tile)
     , filled (Color.rgba 0 0 0 0.2) (triangle size) |> rotate (degrees 90)
     ]

corner color size =
  group
    [ filled color (circle (size / 2) )
    , filled color (rect size (size/2) ) |>  move (0, size / 4)
    , filled color (rect (size/2) size ) |>  move (size / 4, 0)
    -- , filled (Color.rgba 0 0 0 0.2) (triangle size) |> rotate (degrees 90)
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
