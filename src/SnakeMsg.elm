module SnakeMsg exposing ( .. )
import Time exposing ( Time )

type Msg
    = Start
    | NoOp
    | ChangeDirection String
    | Tick Time
    | NewFruit ( Int, Int )
