module Difficulty exposing (view, new, update, distrs, Msg, Difficulty)

import Svg exposing (text, svg, rect, text_, image)
import Html as H exposing (Html, p, label, div)
import Html.Attributes as HA
import Svg.Attributes exposing (width, height, x, y, class, rx, ry, fill, textLength, style, viewBox, textAnchor, id, clipPath, xlinkHref)
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Mouse exposing (Event)

type Difficulty =
    Difficulty
        { distribution : (Float, Float, Float)
        , movingDongle : Maybe Dongle
        }

type Dongle
    = Left
    | Right

type Msg
    = Down Float
    | Move Float
    | Up

sliderWidth : number
sliderWidth = 380

new : Difficulty
new = Difficulty
    { distribution = (1,90,10)
    , movingDongle = Nothing
    }

distrs : Difficulty -> (Float, Float, Float)
distrs (Difficulty { distribution }) = distribution


sliderView : Difficulty -> Html Msg
sliderView (Difficulty { distribution }) =
    let
        (e, n, h) = distribution
        percent x = ceiling <| x * 100 / (e + n + h)
        easyOffset = percent e
        hardOffset = percent (e + n)

        percentView val =
            text_ [ textLength "100%" ] [ text <| formatpc val ]

        genRect class_ val offset =
            [ rect
                [ width <| formatpc (val - 2)
                , x <| formatpc offset
                , y "45%" , height "50%" , rx "4" , ry "4"
                , class class_
                ] []
            , Svg.clipPath [ id class_ ]
                [ rect
                    [ width <| formatpc (val - 2)
                    , x <| formatpc offset, height "100%"
                    ] []
                ]
            , text_
                [ x <| formatpc <| offset + (val // 2) - 1
                , textAnchor "middle"
                , y "82%"
                , style "font:25px sans-serif;fill:#282c34;"
                , clipPath <| "url(#" ++ class_ ++ ")"
                ] [ text <| formatpc val ]
            , text_
                [ x <| formatpc offset
                , y "30%"
                , clipPath <| "url(#" ++ class_ ++ ")"
                ] [ text class_ ]
            ]

        formatpc val =
            String.fromInt val ++ "%"

        easySquare   = genRect "easy"   (percent e) 0
        normalSquare = genRect "normal" (percent n) easyOffset
        hardSquare   = genRect "hard"   (percent h) hardOffset

        dongle loc =
            image
                [ xlinkHref "/icons/bardongle.svg"
                , x <| formatpc <| percent loc - 3
                , height "60%"
                , width "15"
                , y "40%"
                ] []

        dongles = [ dongle e, dongle (e + n) ]

        pointerScreen =
            rect
                [ width "100%", height "60%", fill "transparent",  y "40%"
                , Pointer.onDown <| Down << Tuple.first << .offsetPos << .pointer
                , Pointer.onMove <| Move << Tuple.first << .offsetPos << .pointer
                , Pointer.onUp <| always Up
                , Pointer.onLeave <| always Up
                ] []
    in
        svg
            [ class "dslider"
            , viewBox ("0 0 " ++ String.fromInt sliderWidth ++ " 80")
            ]
            (easySquare ++ normalSquare ++ hardSquare ++ dongles ++ [ pointerScreen ])

view : Difficulty -> Html Msg
view d =
    let
        slider = sliderView d
    in
        div []
            [ p [ HA.class "title" ] [ label [] [ H.text "Difficulty:" ] ]
            , slider
            , div [ HA.class "descr" ]
                [ H.text "Each category has different pools of words for easy, normal and hard words. You can choose how often difficult words show up. Depending on which categories are selected, there may be no easy words." ]
            ]

moveDongle : Float -> Difficulty -> Difficulty
moveDongle location (Difficulty d) =
    let
        (e, n, h)  = d.distribution
        totalDistr = e + n + h
        relLoc     = location / sliderWidth
        locDistr   = relLoc * totalDistr
    in
        Difficulty <|
            { d | distribution = case d.movingDongle of
                Nothing -> d.distribution
                Just Left  ->
                    if totalDistr - h - locDistr < 0 then
                        d.distribution
                    else
                        (locDistr, totalDistr - h - locDistr, h)
                Just Right ->
                    if locDistr - e < 0 then
                        d.distribution
                    else
                        (e, locDistr - e, totalDistr - locDistr)
            }

enableDongle : Float -> Difficulty -> Difficulty
enableDongle location (Difficulty d) =
    let
        (e, n, h)  = d.distribution
        totalDistr = e + n + h
        relLoc     = location / sliderWidth
        midPoint   = (e + e + n) / totalDistr / 2
        closestDongle = if relLoc > midPoint then Right else Left
        difficulty = Difficulty { d | movingDongle = Just closestDongle }
    in
        moveDongle location difficulty

update : Msg -> Difficulty -> Difficulty
update msg (Difficulty d) =
    case msg of
        Up ->
            (Difficulty { d | movingDongle = Nothing })
        Move location ->
            moveDongle location (Difficulty d)
        Down location ->
            enableDongle location (Difficulty d)
