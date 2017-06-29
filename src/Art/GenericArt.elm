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
import Art.Toolbox as Toolbox


type alias PaintingArea s m =
    { canvas : Canvas
    , toolbox : Canvas.Tool Toolbox.State Toolbox.Msg
    , input : Canvas.Input s m
    }


type Art s m
    = Art (PaintingArea s m)


type Msg m
    = InputMsg m
    | CanvasMsg Canvas.Msg
    | ToolboxMsg Toolbox.Msg


update : Msg m -> Art s m -> ( Art s m, Cmd (Msg m) )
update msg (Art pa) =
    case msg of
        InputMsg msg ->
            Canvas.mapInput (\x -> Art { pa | input = x }) CanvasMsg msg pa.input

        CanvasMsg msg ->
            let
                map canvas =
                    ( Art { pa | canvas = canvas }, Cmd.none )
            in
                map <| Canvas.update msg pa.canvas

        ToolboxMsg msg ->
            Canvas.mapInput (\x -> Art { pa | toolbox = x }) CanvasMsg msg pa.toolbox


view : Art s m -> Html (Msg m)
view (Art { canvas, toolbox }) =
    div []
        [ div [ id "drawingcontainer" ] [ Canvas.view canvas ]
        , div [ id "toolbox" ] [ Canvas.viewTool ToolboxMsg toolbox ]
        ]


subs : Art s m -> Sub (Msg m)
subs (Art { input }) =
    Sub.batch
        [ Canvas.subInput InputMsg input
        ]
