module Position exposing
  ( Vector
  , vecEql
  , detectCollisions
  , Dimensions
  , wrapPosition
  )
import Utils.Math exposing (wrap)
import Tile exposing (tile, tiles, world)

type alias Vector =
  { x: Float, y: Float }

type alias Dimensions =
  { width: Float, height: Float }


vecEql : Vector -> Vector -> Bool
vecEql vecA vecB =
  vecA.x == vecB.x &&
  vecA.y == vecB.y

detectCollisions : List Vector -> Bool
detectCollisions body =
  case List.tail body of
    Nothing -> False
    Just tail ->
      case List.head body of
        Nothing -> False
        Just head -> detectCollision head tail

detectCollision : Vector -> List Vector -> Bool
detectCollision head tail =
  let
    collided =
      List.foldl
        (\tile m -> m || vecEql head tile)
        False
        tail
  in
    if collided then True else detectCollisions tail

wrapPosition : Dimensions -> Vector -> Vector
wrapPosition world position =
  Vector
    (wrap position.x tiles)
    (wrap position.y tiles)
