module Difficulty exposing (view, new, Msg, Difficulty(..))

import Svg exposing (text, svg, rect)
import Html exposing (Html, p, label, text, div)
import Html.Attributes as HA exposing (href)
import Svg.Attributes exposing (width, height, x, y, class, rx, ry)

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
        genRect class_ x_ width_ =
            rect
                [ percent width width_
                , percent x x_
                , height "100%"
                , rx "4"
                , ry "4"
                , class class_
                ] []

        percent f val =
            f (String.fromInt val ++ "%")

        leftDongle offset =
            e * 100 / (e + n + h)
                |> ceiling
                |> (+) offset

        rightDongle offset =
            (e + n) * 100 / (e + n + h)
                |> ceiling
                |> (+) offset

        easySquare =
            genRect "easy" 0 (leftDongle -1)
        normalSquare =
            genRect "normal" (leftDongle 1) (rightDongle -1 - leftDongle 1)
        hardSquare =
            genRect "hard" (rightDongle 1) (100 - rightDongle 1)
    in
        svg [ class "dslider" ] [ easySquare, normalSquare, hardSquare ]

view : Difficulty -> Html Msg
view d =
    let
        slider = sliderView d
    in
        div []
            [ p [ HA.class "title" ] [ label [] [ text "Difficulty:" ] ]
            , slider
            , div [ HA.class "descr" ]
                [ text "Each category has different pools of words for easy, normal and hard words. You can choose how often difficult words show up." ]
            ]
