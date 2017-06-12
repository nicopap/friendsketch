module Art exposing (view, update, subs, Msg, Art, new)

import Html exposing (..)
import Html.Attributes exposing (..)
import Art.Canvas as Canvas
import Art.RenderedCanvas as RenderedCanvas
import Art.Pen.Mouse as Mouse
import Art.Pen.Remote as Remote
import Art.Pen as Pen
import Art.Toolbox as Toolbox
import Art.ToolboxMsg as TBM


type alias ThumbState =
    Never


type alias TabletState =
    Never


type Input
    = MouseIn Mouse.State
    | ThumbIn ThumbState
    | TabletIn TabletState


type ArtState
    = Viewing Remote.State
    | Painting Input
    | Invisible


type alias Art =
    { canvas : Canvas.Canvas
    , state : ArtState
    , toolbox : Toolbox.Toolbox
    }


type Msg
    = MouseMsg Mouse.Msg
    | PenMsg Pen.Msg
    | RemoteMsg Never
    | ToolboxMsg TBM.ToolboxMsg


new : Art
new =
    Art Canvas.new (Painting (MouseIn Mouse.newState)) (Toolbox.new)


subs : Art -> Sub Msg
subs art =
    case art.state of
        --Sub.map RemoteMsg (remote.subs art.canvas)
        Viewing remote ->
            Sub.none

        Painting (MouseIn state) ->
            Sub.batch
                [ Sub.map MouseMsg (Mouse.subs state)
                , Sub.map PenMsg (Pen.subs)
                ]

        Painting (ThumbIn _) ->
            Sub.none

        Painting (TabletIn _) ->
            Sub.none

        Invisible ->
            Sub.none


view : Art -> Html Msg
view { canvas, toolbox } =
    div []
        [ div [ id Canvas.id ] [ RenderedCanvas.htmlCanvas canvas ]
        , div [ id "toolbox" ] [ Html.map ToolboxMsg (Toolbox.view toolbox) ]
        ]


update : Msg -> Art -> ( Art, Cmd Msg )
update msg art =
    let
        oldmousestate =
            case art.state of
                Painting (MouseIn state) ->
                    state

                _ ->
                    Mouse.newState

        mousemap mousemsg ( mousestate, penmsg ) =
            ( { art | state = Painting (MouseIn mousestate) }
            , Cmd.map PenMsg penmsg
            )

        remotemap ( remotestate, remotemsg ) =
            ( { art | state = Viewing remotestate }
            , Cmd.map RemoteMsg remotemsg
            )

        penmap ( canvas, penmsg ) =
            ( { art | canvas = canvas }, Cmd.map PenMsg penmsg )

        toolboxmap : Toolbox.ToolboxAnswer -> ( Art, Cmd Msg )
        toolboxmap { canvas, msg, toolbox } =
            ( { art | canvas = canvas, toolbox = toolbox }
            , Cmd.map ToolboxMsg msg
            )
    in
        case msg of
            MouseMsg mousemsg ->
                Mouse.update mousemsg oldmousestate |> mousemap mousemsg

            RemoteMsg remotemsg ->
                Remote.update remotemsg art.canvas |> remotemap

            PenMsg penmsg ->
                Pen.update penmsg art.canvas |> penmap

            ToolboxMsg toolboxmsg ->
                Toolbox.update toolboxmsg art.canvas art.toolbox |> toolboxmap
