module Art.GenericArt
    exposing
        ( update
        , view
        , subs
        , Msg
        , Art(..)
        )

{-| Module to handle the drawing area in the netpinary application.

This is purely generic. The concrete Art type must specify what is the input
method. Thanks to the Elm Architecture, this also means that the concrete Art
implementation gains a free transparent switching of input type by just
defining functions that translates from one concrete type to the other.

-}

import Html exposing (button, div, text, Html)
import Html.Attributes exposing (id)
import Art.Canvas as Canvas exposing (Canvas)
import Art.Box as Box exposing (Box)
import Art.Pen as Pen
import Art.Toolbox as Toolbox exposing (Toolbox)
import Art.ToolboxMsg as TBM


type alias PaintingArea s m =
    { canvas : Canvas
    , toolbox : Toolbox
    , input : Pen.Input s m
    }


type Art s m
    = Art (PaintingArea s m)


type Msg m
    = InputMsg m
    | PenMsg Pen.Msg
    | ToolboxMsg TBM.ToolboxMsg
    | UpdatePosition Box


update : Msg m -> Art s m -> ( Art s m, Cmd (Msg m) )
update msg (Art pa) =
    case msg of
        UpdatePosition box ->
            let
                map canvas =
                    ( Art { pa | canvas = canvas }, Cmd.none )
            in
                map <| Canvas.setLocation box pa.canvas

        InputMsg msg ->
            let
                map ( input, cmd ) =
                    ( Art { pa | input = input }, Cmd.map PenMsg cmd )
            in
                map <| Pen.updateInput msg pa.input

        PenMsg msg ->
            let
                map canvas =
                    ( Art { pa | canvas = canvas }, Cmd.none )
            in
                map <| Pen.update msg pa.canvas

        ToolboxMsg msg ->
            let
                map { msg, canvas, toolbox } =
                    ( Art { pa | canvas = canvas, toolbox = toolbox }
                    , Cmd.batch
                        [ Cmd.map ToolboxMsg msg
                        , Box.checkCanvas ()
                        ]
                    )
            in
                map <| Toolbox.update msg pa.canvas pa.toolbox


view : Art s m -> Html (Msg m)
view (Art { canvas, toolbox }) =
    div []
        [ div [ id "drawingcontainer" ] [ Canvas.view canvas ]
        , div [ id "toolbox" ] [ Html.map ToolboxMsg <| Toolbox.view toolbox ]
        ]


subs : Art s m -> Sub (Msg m)
subs (Art { input }) =
    Sub.batch
        [ Sub.map InputMsg <| input.subs input.state
        , Box.sub UpdatePosition
        ]
