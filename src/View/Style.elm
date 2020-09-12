module View.Style exposing (abel, black, blue, ebg, fadeIn, grey, popIn, purple, rotate, sand, serif, varela, white, yellow)

import Element exposing (Attribute, Color, rgb255)
import Element.Font as Font
import Helpers.View exposing (style)


popIn : Attribute msg
popIn =
    style "animation" "enter 0.5s"


fadeIn : Attribute msg
fadeIn =
    style "animation" "fadeIn 0.5s"


serif : Attribute msg
serif =
    Font.family
        [ Font.typeface "Helvetica"
        ]


abel : Attribute msg
abel =
    Font.family
        [ Font.typeface "Abel"
        ]


ebg : Attribute msg
ebg =
    Font.family
        [ Font.typeface "EB Garamond"
        ]


varela : Attribute msg
varela =
    Font.family
        [ Font.typeface "Varela"
        ]


rotate : Attribute msg
rotate =
    style
        "animation"
        "rotation 0.7s infinite linear"


blue : Color
blue =
    rgb255 112 237 204


purple : Color
purple =
    rgb255 145 130 254


white : Color
white =
    rgb255 255 255 255


yellow : Color
yellow =
    rgb255 255 238 147


black : Color
black =
    rgb255 0 0 0


sand : Color
sand =
    rgb255 255 245 235


grey : Color
grey =
    rgb255 150 150 150
