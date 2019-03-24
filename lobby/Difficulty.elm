module Difficulty exposing (view, new, Msg, Difficulty(..))

import Svg exposing (text, svg, rect, text_)
import Html as H exposing (Html, p, label, div)
import Html.Attributes as HA exposing (href)
import Svg.Attributes exposing (width, height, x, y, class, rx, ry, fill, textLength, style, viewBox, textAnchor, id, clipPath)

type Difficulty = Difficulty Float Float Float

type Msg
    = DragLeft
    | DragRight

new : Difficulty
new =
     Difficulty 0.33 0.33 0.33
sliderView : Difficulty -> Html Msg
sliderView (Difficulty e n h) =
    let
        percent x = ceiling <| x * 100 / (e + n + h)
        easyOffset = percent e
        hardOffset = percent (e + n)

        percentView val =
            text_ [ textLength "100%" ] [ text <| formatpc val ]

        genRect class_ val offset =
            [ rect
                [ width <| formatpc (val - 2)
                , x <| formatpc offset
                , y "45%" , height "55%" , rx "4" , ry "4"
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
    in
        svg [ class "dslider", viewBox "0 0 380 80" ]  <|
            easySquare ++ normalSquare ++ hardSquare

view : Difficulty -> Html Msg
view d =
    let
        slider = sliderView d
    in
        div []
            [ p [ HA.class "title" ] [ label [] [ H.text "Difficulty:" ] ]
            , slider
            , div [ HA.class "descr" ]
                [ H.text "Each category has different pools of words for easy, normal and hard words. You can choose how often difficult words show up." ]
            ]
