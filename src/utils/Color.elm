module Utils.Color exposing (fruitColor, ringColor)
import Utils.Math exposing (..)
import Color exposing (..)
import TypeList exposing (..)

fruitColor:  Int -> Int -> Color
fruitColor bonus maxBonus =
  greyedColor Red (lerpFromPosition 100 200 bonus maxBonus)

ringColor: Int -> Int -> Color
ringColor idx length =
  greyedColor Green (lerpFromPosition 100 200 idx length)

lerpFromPosition: Float -> Float -> Int -> Int -> Int
lerpFromPosition min max idx length =
  round (lerp min max (toFloat idx / toFloat length))


greyedColor: TileColor -> Int -> Color
greyedColor color val =
  let
    grey = val // 3
  in
    case color of
      Red -> rgb val grey grey
      Green -> rgb grey val grey
      Blue -> rgb grey grey val
