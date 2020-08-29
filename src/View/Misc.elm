module View.Misc exposing (btn, btn2, btn3, formatDay, icon, isTall, isWide, lnk, monthToString, spinner)

import Date exposing (Date)
import Element exposing (Element, el, fill, height, html, none, padding, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Helpers.View exposing (style, whenAttr)
import Material.Icons.Types exposing (Coloring(..), Icon)
import Ordinal
import Time exposing (Month(..))
import Types exposing (Screen)
import View.Style exposing (black, blue, grey, rotate, varela, white)


isTall : Screen -> Bool
isTall scr =
    scr.height >= 660


isWide : Screen -> Bool
isWide scr =
    scr.width > 768


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


icon : Icon msg -> Int -> Element msg
icon ic n =
    ic n Inherit
        |> Element.html
        |> el []


lnk : String -> msg -> Element msg
lnk txt msg =
    Input.button
        [ Font.underline
        , Font.italic
        , Font.size 16
        , Element.mouseOver
            [ Font.color blue
            ]
        ]
        { onPress = Just msg
        , label = text txt
        }


btn2 : Bool -> Icon msg -> String -> msg -> Element msg
btn2 inProg ic str msg =
    Input.button
        [ padding 10

        --, style "transition" "all 0.2s"
        , Element.mouseOver
            [ Font.color white
            , Background.color black
            ]
        , Border.rounded 15
        , Background.color grey
        , style "cursor" "wait"
            |> whenAttr inProg
        , Font.color black
        , height <| px 50
        , width <| px 120
        , Border.shadow
            { offset = ( 3, 3 )
            , blur = 4
            , size = 2
            , color = Element.rgb255 150 150 150
            }
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
                ic 26 Inherit
                    |> Element.html
                    |> el []
            , text str
            ]
                |> row [ spaceEvenly, width fill ]
        }


btn3 : Bool -> Icon msg -> String -> msg -> Element msg
btn3 inProg ic str msg =
    Input.button
        [ Element.paddingXY 15 0
        , Element.mouseOver
            [ Font.color white
            , Background.color black
            ]
        , style "cursor" "wait"
            |> whenAttr inProg
        , Font.color black
        , height <| px 50
        , Border.rounded 25
        , Background.color grey
        , varela
        , Border.shadow
            { offset = ( 3, 3 )
            , blur = 3
            , size = 0
            , color = Element.rgb255 150 150 150
            }
        ]
        { onPress =
            if inProg then
                Nothing

            else
                Just msg
        , label =
            [ if inProg then
                spinnerN 20

              else
                icon ic 20
            , text str
            ]
                |> row [ spacing 10 ]
        }


btn : String -> msg -> Element msg
btn str msg =
    Input.button
        [ Element.paddingXY 15 0

        --, style "transition" "all 0.2s"
        , Element.mouseOver
            [ Font.color white
            , Background.color black
            ]
        , Border.color black
        , Border.width 1
        , height <| px 50
        ]
        { onPress = Just msg
        , label = text str
        }


spinner : Element msg
spinner =
    spinnerN 26


spinnerN : Int -> Element msg
spinnerN n =
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
