module SnakeKeyboard exposing ( keyboardProcessor )
import SnakeMsg exposing (..)

--------------
-- KEY HELPERS
--------------

type Key
  = Space
  | ArrowLeft
  | ArrowRight
  | ArrowDown
  | ArrowUp
  | Unknown

fromCode : Int -> Key
fromCode keyCode =
  case keyCode of
    32 -> Space
    -- arrows
    38 -> ArrowUp
    39 -> ArrowRight
    40 -> ArrowDown
    37 -> ArrowLeft
    -- aswd
    87 -> ArrowUp
    68 -> ArrowRight
    83 -> ArrowDown
    65 -> ArrowLeft
    _ -> Unknown

keyboardProcessor keyCode =
  case fromCode keyCode of
    Space -> SnakeMsg.Start
    ArrowDown -> SnakeMsg.ChangeDirection "Down"
    ArrowLeft -> SnakeMsg.ChangeDirection "Left"
    ArrowRight -> SnakeMsg.ChangeDirection "Right"
    ArrowUp -> SnakeMsg.ChangeDirection "Up"
    _ -> SnakeMsg.NoOp
