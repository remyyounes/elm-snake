module Utils.Math exposing (lerp, wrap)

lerp : number -> number -> number -> number
lerp min max val = min + ((max - min) * val)

wrap : Float -> Int -> Float
wrap v range =
  let
    min = 0
    max = range - 1
  in
    if v < min then
      max
    else if v > max then
      min
    else
      v
