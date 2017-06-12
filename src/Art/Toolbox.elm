module Art.Toolbox exposing (view, update, Toolbox, ToolboxAnswer, new)

import Html
import Art.ToolboxMsg exposing (ToolboxMsg(..))
import Art.Canvas as Canvas
import Art.Tools.ColorPicker as ColorPicker
import Art.Tools.SizePicker as SizePicker


type alias ToolboxAnswer =
    { msg : Cmd ToolboxMsg
    , canvas : Canvas.Canvas
    , toolbox : Toolbox
    }


type alias Toolbox =
    ()


new : Toolbox
new =
    ()


update : ToolboxMsg -> Canvas.Canvas -> Toolbox -> ToolboxAnswer
update msg canvas toolbox =
    let
        newcanvas : Canvas.Canvas
        newcanvas =
            case msg of
                ChangeColor newcolor ->
                    { canvas | color = newcolor }

                ChangeSize newsize ->
                    { canvas | strokeSize = newsize }
    in
        { msg = Cmd.none, toolbox = toolbox, canvas = newcanvas }


view : Toolbox -> Html.Html ToolboxMsg
view _ =
    Html.div []
        [ ColorPicker.view
        , SizePicker.view
        ]
