module Art.Pen.Mouse exposing (newInput, State, Msg)

import Mouse
import Art.Box as Box exposing (Box, Point)
import Art.Canvas as Canvas


type Msg
    = Hover Mouse.Position
    | Lift
    | Press Mouse.Position
    | SubCanvasLoc Point


type State_
    = Sleeping
    | Activated


type alias State =
    { state : State_
    , canvasOffset : Point
    }


toCanvasCoord : Point -> Int -> Int -> Point
toCanvasCoord canvasOffset x y =
    { x = toFloat x - canvasOffset.x
    , y = canvasOffset.y - toFloat y
    }


update : Msg -> State -> ( State, Cmd Canvas.Msg )
update msg { state, canvasOffset } =
    case msg of
        Press { x, y } ->
            { state = Activated, canvasOffset = canvasOffset }
                ! [ Canvas.press <| toCanvasCoord canvasOffset x y
                  , Box.checkCanvas ()
                  ]

        Lift ->
            ( { state = Sleeping, canvasOffset = canvasOffset }
            , Canvas.lift
            )

        Hover { x, y } ->
            ( { state = state, canvasOffset = canvasOffset }
            , Canvas.hover <| toCanvasCoord canvasOffset x y
            )

        SubCanvasLoc point ->
            ( { state = state, canvasOffset = point }
            , Cmd.none
            )


subs : State -> Sub Msg
subs _ =
    Sub.batch
        [ Mouse.moves Hover
        , Mouse.ups <| always Lift
        , Mouse.downs Press
        , Box.sub (SubCanvasLoc << (\{ x, y } -> { x = x, y = y }))
        ]


newInput : Canvas.Input State Msg
newInput =
    { state = { state = Sleeping, canvasOffset = { x = 0, y = 0 } }
    , update = update
    , subs = subs
    }
