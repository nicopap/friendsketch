module Art.ToolboxMsg exposing (ToolboxMsg(..))

import Color


type ToolboxMsg
    = ChangeColor Color.Color
    | ChangeSize Float
