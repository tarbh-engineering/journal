module View.Misc exposing (btn, btn2, ebg, formatDay, monthToString, rotate, spinner, varela)

import Date exposing (Date)
import Element exposing (Attribute, Element, el, fill, height, html, none, padding, px, row, spaceEvenly, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Helpers.View exposing (style, whenAttr)
import Material.Icons.Types exposing (Coloring(..), Icon)
import Ordinal
import Time exposing (Month(..))
import View.Style exposing (black, white)


formatDay : Date -> String
formatDay d =
    [ d |> Date.day |> Ordinal.ordinal
    , d
        |> Date.month
        |> monthToString
    , d |> Date.year |> String.fromInt
    ]
        |> String.join " "


monthToString : Month -> String
monthToString month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


btn2 : Bool -> Icon msg -> String -> msg -> Element msg
btn2 inProg icon str msg =
    Input.button
        [ padding 10
        , style "transition" "all 0.2s"
        , Element.mouseOver
            [ Font.color white
            , Background.color black
            ]
        , Border.color black
        , Border.width 1
        , style "cursor" "wait"
            |> whenAttr inProg
        , Font.color black
        , height <| px 50
        , width <| px 120
        ]
        { onPress =
            if inProg then
                Nothing

            else
                Just msg
        , label =
            [ if inProg then
                spinner

              else
                icon 26 Inherit
                    |> Element.html
                    |> el []
            , text str
            ]
                |> row [ spaceEvenly, width fill ]
        }


btn : String -> msg -> Element msg
btn str msg =
    Input.button
        [ padding 15
        , style "transition" "all 0.2s"
        , Element.mouseOver
            [ Font.color white
            , Background.color black
            ]
        , Border.color black
        , Border.width 1
        ]
        { onPress = Just msg
        , label = text str
        }


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


spinner : Element msg
spinner =
    let
        n =
            26
    in
    [ none
        |> el [ width fill, height fill, Background.color white ]
    , none
        |> el [ width fill, height fill, Background.color black ]
    ]
        |> row
            [ width <| px n
            , height <| px n
            , Border.rounded <| n // 2
            , Border.width 1
            , rotate
            , Element.clip
            , Border.color black
            ]
