module View.Style exposing (abel, baseFont, black, blue, ebg, fadeIn, garish, gold, green, grey, paper, paperAlpha, popIn, purple, red, rotate, sand, serif, shadow, shadowAlt, shadowNone, text, white, yellow)

import Element exposing (Attribute, Color, Element, el, rgb255)
import Element.Border as Border
import Element.Font as Font
import Helpers.View exposing (style)


shadow : Attribute msg
shadow =
    Border.shadow
        { offset = ( 3, 3 )
        , blur = 0
        , size = 0
        , color = grey
        }


shadowAlt : Attribute msg
shadowAlt =
    Border.shadow
        { offset = ( 3, 3 )
        , blur = 3
        , size = 1
        , color = grey
        }


shadowNone : Element.Attr dec msg
shadowNone =
    Border.shadow
        { offset = ( 0, 0 )
        , blur = 0
        , size = 0
        , color = grey
        }


popIn : Attribute msg
popIn =
    style "animation" "enter 0.3s"


fadeIn : Attribute msg
fadeIn =
    style "animation" "fadeIn 0.5s"


text : String -> Element msg
text =
    Element.text
        >> el []


abel : Attribute msg
abel =
    Font.family
        [ Font.typeface "Abel"
        ]


serif : String -> Element msg
serif =
    Element.text
        >> el
            [ ebg
            ]


baseFont : Attribute msg
baseFont =
    Font.family
        [ Font.typeface "Quicksand"
        ]


ebg : Attribute msg
ebg =
    Font.family
        [ Font.typeface "EB Garamond"
        ]


rotate : Attribute msg
rotate =
    style
        "animation"
        "rotation 0.7s infinite linear"


gold : Color
gold =
    rgb255 212 196 78


blue : Color
blue =
    rgb255 51 103 211


red : Color
red =
    rgb255 206 68 40


green : Color
green =
    rgb255 108 164 50


garish : Color
garish =
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
    rgb255 255 223 145


grey : Color
grey =
    rgb255 150 150 150


paper : Color
paper =
    paperAlpha 1.0


paperAlpha : Float -> Color
paperAlpha =
    Element.rgba255 220 240 255
