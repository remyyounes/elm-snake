module Position exposing
  ( vecEql
  , detectCollisions
  )
import Tile exposing (tile, tiles, world)

vecEql vecA vecB =
  vecA.x == vecB.x &&
  vecA.y == vecB.y

detectCollisions body =
  case List.tail body of
    Nothing -> False
    Just tail ->
      case List.head body of
        Nothing -> False
        Just head -> detectCollision head tail

detectCollision head tail =
  let
    collided =
      List.foldl
        (\tile m -> m || vecEql head tile)
        False
        tail
  in
    if collided then True else detectCollisions tail
