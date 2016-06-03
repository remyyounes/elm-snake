module TypeList exposing (..)
import Time exposing ( Time )
--------
-- Types
--------

type alias Vector =
  { x: Float, y: Float }

type alias Dimensions =
  { width: Float, height: Float }

type alias Snake =
  { pos: Vector, body: List Vector }


type TileColor
  = Red
  | Green
  | Blue

type GameState
  = Over
  | Playing
