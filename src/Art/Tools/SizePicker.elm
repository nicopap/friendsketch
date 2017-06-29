module Art.Tools.SizePicker exposing (newTool, Msg, State)

import Html exposing (Html)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput)
import Json.Decode as Json
import Art.Canvas as Canvas


type Msg
    = ChangeSize Float


type alias State =
    { size : Float
    }


decodeString : String -> Float
decodeString input =
    case Json.decodeString Json.float input of
        Ok val ->
            val

        _ ->
            10


update : Msg -> State -> ( State, Cmd Canvas.Msg )
update msg state =
    case msg of
        ChangeSize newsize ->
            ( { state | size = newsize }
            , Canvas.changePenSize newsize
            )


slider : Html Msg
slider =
    Html.input [ type_ "range", onInput (ChangeSize << decodeString) ] []


view : State -> Html Msg
view =
    always slider


newTool : Canvas.Tool State Msg
newTool =
    { state = { size = 10 }
    , update = update
    , view = view
    }
