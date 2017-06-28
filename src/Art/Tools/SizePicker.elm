module Art.Tools.SizePicker exposing (view)

import Html exposing (Html)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput)
import Json.Decode as Json
import Art.ToolboxMsg exposing (ToolboxMsg(..))


decodeString : String -> Float
decodeString input =
    case Json.decodeString Json.float input of
        Ok val ->
            val

        _ ->
            10


slider : Html ToolboxMsg
slider =
    Html.input [ type_ "range", onInput (ChangeSize << decodeString) ] []


view : Html ToolboxMsg
view =
    slider
