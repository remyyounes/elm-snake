module Fruit exposing (init, update, view, Fruit)
import Position exposing (Vector)
import Utils.Color exposing (fruitColor)
import Collage exposing (..)
import Color exposing (..)
import Tile
type alias Fruit =
  { pos: Vector, bonus: Int, maxBonus: Int}

maxBonus : Int
maxBonus = 100

init : Int -> Int -> Fruit
init x y =
  { pos = Vector (toFloat x) (toFloat y)
  , bonus = maxBonus
  , maxBonus = maxBonus }

update : Fruit -> Fruit
update fruit =
  { fruit | bonus = fruit.bonus - 1 }

view : Fruit -> Form
view fruit =
  Tile.viewFruit (fruitColor fruit.bonus fruit.maxBonus) fruit.pos ""
