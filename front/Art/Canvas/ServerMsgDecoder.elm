module Art.Canvas.ServerMsgDecoder
    exposing
        ( ServerMsg(..)
        , endEncoder
        , continueEncoder
        , startEncoder
        , decode
        )

{-| This module holds all decoders and encoders related to canvas remote
manipulation. It doesn't provide any form of encapsulation, as it exposes
the constructors for the ServerMsg data type.
-}

import Color exposing (Color)
import List
import Result
import Json.Encode as Encode exposing (encode)
import Json.Decode as Decode exposing (Decoder, decodeString)
import TypeDecoders exposing (..)
import ColorMath as ColMath
import ElementRelativeMouseEvents as MouseE exposing (Point)


type ServerMsg
    = Start Point Color Float
    | Continue Point
    | End
    | DecodeError String



-- DECODERS


decode : (ServerMsg -> a) -> String -> a
decode continuation input =
    let
        handleResult r =
            case r of
                Ok msg ->
                    msg

                Err err ->
                    DecodeError err
    in
        decodeString canvasActionDecoder input
            |> handleResult
            |> continuation


pointDecoder : Decoder Point
pointDecoder =
    Point <*| 0 :^ Decode.float |*| 1 :^ Decode.float


colorDecoder : Decoder Color
colorDecoder =
    Decode.map
        (\x -> Result.withDefault Color.black (ColMath.hexToColor x))
        Decode.string


startEncoder : Point -> Color -> Float -> String
startEncoder { x, y } color size =
    Encode.list
        [ Encode.list [ Encode.float x, Encode.float y ]
        , Encode.string <| ColMath.colorToHex color
        , Encode.float size
        ]
        |> (,) "start"
        |> List.singleton
        |> Encode.object
        |> encode 0


continueEncoder : Point -> String
continueEncoder { x, y } =
    encode 0 <|
        Encode.object
            [ ( "continue", Encode.list [ Encode.float x, Encode.float y ] ) ]


endEncoder : String
endEncoder =
    encode 0 <| Encode.object [ ( "end", Encode.null ) ]


canvasActionDecoder : Decoder ServerMsg
canvasActionDecoder =
    sumType
        <+| "start" := Start
            <*| 0 :^ pointDecoder
            |*| 1 :^ colorDecoder
            |*| 2 :^ Decode.float
        |+| "continue" := Continue <*| pointDecoder
        |+> "end" :- End
