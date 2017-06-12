module Art.Tools.SizePicker exposing (view)

import Html
import Html.Events exposing (onClick)
import Art.ToolboxMsg exposing (ToolboxMsg(..))


sizes =
    [ 5, 10, 15, 20, 30, 50, 110 ]


labels =
    [ "tiny", "small", "medium", "large", "huge", "humongus" ]


sizeButton : Float -> String -> Html.Html ToolboxMsg
sizeButton size text =
    Html.button [ onClick (ChangeSize size) ] [ Html.text text ]


view : Html.Html ToolboxMsg
view =
    Html.div []
        (List.map2 sizeButton sizes labels)
