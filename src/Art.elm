module Art exposing (new, update, view, subs, Art, Msg)

{-| Provides an interface to the GenericArt construct.

It exposes only Concrete types. It also defines initialization and
transformation functions tied to those types.

-}

import Html exposing (button, text, Html)
import Html.Events exposing (onClick)
import Art.GenericArt as GArt
import Art.Canvas as Canvas
import Art.Toolbox as Toolbox
import Art.Pen.Mouse as Mouse
import Art.Pen.Remote as RemotePen


type alias Remote = GArt.Art RemotePen.State RemotePen.Msg
type alias Local = GArt.Art Mouse.State Mouse.Msg
type alias RemoteMsg = GArt.Msg RemotePen.Msg
type alias LocalMsg = GArt.Msg Mouse.Msg


type Art
    = Sremote Remote
    | Slocal Local
    | None


type Msg
    = StartRemote
    | StartLocal
    | Hide
    | RMsg RemoteMsg
    | LMsg LocalMsg


newRemote : Art
newRemote =
    Sremote <|
        GArt.Art
            { canvas = Canvas.new
            , toolbox = Toolbox.new
            , input = RemotePen.newInput
            }


newLocal : Art
newLocal =
    Slocal <|
        GArt.Art
            { canvas = Canvas.new
            , toolbox = Toolbox.new
            , input = Mouse.newInput
            }


new : Art
new = newLocal


rupdate : RemoteMsg -> Art -> ( Art, Cmd Msg )
rupdate msg art =
    case art of
        Sremote remote ->
            let
                map ( art_, cmd ) =
                    ( Sremote art_, Cmd.map RMsg cmd )
            in
                map <| GArt.update msg remote

        Slocal _ ->
            Debug.crash "Local art in Remote update"

        None ->
            Debug.crash "Invisible art in Remote update"


lupdate : LocalMsg -> Art -> ( Art, Cmd Msg )
lupdate msg art =
    case art of
        Slocal local ->
            let
                map ( art_, cmd ) =
                    ( Slocal art_, Cmd.map LMsg cmd )
            in
                map <| GArt.update msg local

        Sremote _ ->
            Debug.crash "Remote art in Local update"

        None ->
            Debug.crash "Invisible art in Local update"


update : Msg -> Art -> ( Art, Cmd Msg )
update msg art =
    case msg of
        StartRemote ->
            ( newRemote, Cmd.none )

        StartLocal ->
            ( newLocal, Cmd.none )

        Hide ->
            ( None, Cmd.none )

        RMsg msg_ ->
            rupdate msg_ art

        LMsg msg_ ->
            lupdate msg_ art


view : Art -> Html Msg
view art =
    case art of
        Sremote art ->
            Html.map RMsg <| GArt.view art

        Slocal art ->
            Html.map LMsg <| GArt.view art

        None ->
            button [ onClick StartLocal ] [ text "Open Canvas" ]


subs : Art -> Sub Msg
subs art =
    case art of
        Sremote art ->
            Sub.map RMsg <| GArt.subs art

        Slocal art ->
            Sub.map LMsg <| GArt.subs art

        None ->
            Sub.none
