module Tile exposing (tile, tiles, world, view)
import Collage exposing (..)
import Color exposing (..)

tile = 20
tiles = 20
world =
  { width = tiles * tile, height = tiles * tile }

view : Color -> {a | x : Float, y : Float} -> Form
view color position =
  filled color (square tile)
    |>  move (position.x * tile, position.y * tile)
    |>  move (tile / 2, tile / 2)
    |>  move (-world.width / 2, -world.height / 2)
