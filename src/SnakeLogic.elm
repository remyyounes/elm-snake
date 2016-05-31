module SnakeLogic exposing ( lerp, wrap, tailToHead )

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

tailToHead : a -> List a -> List a
tailToHead leader body =
    List.append [leader] (List.take (List.length body - 1) body)
