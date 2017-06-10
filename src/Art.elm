module Art exposing (view, update, subs, Msg, Art, new)

import Html exposing (..)
import Html.Attributes exposing (..)
import Art.Canvas as Canvas
import Art.RenderedCanvas as RenderedCanvas
import Art.Pen.Mouse as Mouse
import Art.Pen.Remote as Remote


type alias Art =
    { canvas : Canvas.Canvas
    , state : ArtState
    }


new : Art
new =
    Art Canvas.new (Painting (MouseIn Mouse.newState))


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


type Msg
    = MouseMsg Mouse.Msg
    | RemoteMsg Never


subs : Art -> Sub Msg
subs art =
    case art.state of
        --Sub.map RemoteMsg (remote.subs art.canvas)
        Viewing remote ->
            Sub.none

        Painting (MouseIn state) ->
            Sub.map MouseMsg (Mouse.subs state)

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
        artmap ( ( mousestate, canvas ), mousemsg ) =
            ( { art
                | canvas = canvas
                , state = Painting (MouseIn mousestate)
              }
            , Cmd.map MouseMsg mousemsg
            )

        remotemap ( ( remotestate, canvas ), remotemsg ) =
            ( { art
                | canvas = canvas
                , state = Viewing remotestate
              }
            , Cmd.map RemoteMsg remotemsg
            )
    in
        case msg of
            MouseMsg drawmsg ->
                artmap (Mouse.update drawmsg art.canvas)

            RemoteMsg remotemsg ->
                remotemap (Remote.update remotemsg art.canvas)
