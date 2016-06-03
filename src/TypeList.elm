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

type alias Fruit =
  { pos: Vector, bonus: Int, maxBonus: Int}

type alias Game =
  { score: Int
  , previousDirection: String
  , direction: String
  , lastFrameDelta: Time
  , fruit: Fruit
  , state: GameState
  , snake: Snake }

type TileColor
  = Red
  | Green
  | Blue

type GameState
  = Over
  | Playing
