module Chat.InputField exposing (onEnter)

import Html exposing (Attribute)
import Html.Events as Event exposing (targetValue, keyCode)
import Json.Decode as Dec

onEnter : msg -> Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Dec.succeed msg
            else
                Dec.fail "not ENTER"

        decodeEnter =
            keyCode |> Dec.andThen isEnter
    in
        Event.on "keydown" decodeEnter


