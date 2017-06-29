module Art.Toolbox exposing (newTool, State, Msg)

import Html exposing (Html)
import Art.Canvas as Canvas exposing (Canvas)
import Art.Tools.ColorPicker as PColor
import Art.Tools.SizePicker as PSize


type Msg
    = CMsg PColor.Msg
    | SMsg PSize.Msg


type State
    = State (Canvas.Tool PColor.State PColor.Msg) (Canvas.Tool PSize.State PSize.Msg)


update : Msg -> State -> ( State, Cmd Canvas.Msg )
update msg_ (State colorTool sizeTool) =
    case msg_ of
        CMsg msg ->
            Canvas.mapInput (\x -> State x sizeTool) identity msg colorTool

        SMsg msg ->
            Canvas.mapInput (State colorTool) identity msg sizeTool


view : State -> Html Msg
view (State colorTool sizeTool) =
    Html.div []
        [ Html.map CMsg <| colorTool.view colorTool.state
        , Html.map SMsg <| sizeTool.view sizeTool.state
        ]


newTool : Canvas.Tool State Msg
newTool =
    { state = State PColor.newTool PSize.newTool
    , update = update
    , view = view
    }
