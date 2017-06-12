module Art exposing (view, update, subs, Msg, Art, new)

import Html exposing (..)
import Html.Attributes exposing (..)
import Art.Canvas as Canvas
import Art.RenderedCanvas as RenderedCanvas
import Art.Pen.Mouse as Mouse
import Art.Pen.Remote as Remote
import Art.Pen as Pen


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
    }


type Msg
    = MouseMsg Mouse.Msg
    | PenMsg Pen.Msg
    | RemoteMsg Never


new : Art
new =
    Art Canvas.new (Painting (MouseIn Mouse.newState))


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


view : Art -> Html msg
view { canvas } =
    div []
        [ div [ id Canvas.id ] [ RenderedCanvas.htmlCanvas canvas ]
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
    in
        case msg of
            MouseMsg drawmsg ->
                mousemap drawmsg (Mouse.update drawmsg oldmousestate)

            RemoteMsg remotemsg ->
                remotemap (Remote.update remotemsg art.canvas)

            PenMsg penmsg ->
                penmap (Pen.update penmsg art.canvas)
