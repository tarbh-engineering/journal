module View.Misc exposing (btn2, btn3, dayParts, formatDateTime, formatDay, getArea, iBtn, icon, lnk, spinner, tallInt)

import Calendar exposing (Date)
import DateTime exposing (DateTime)
import Element exposing (Element, el, fill, height, html, none, padding, px, row, spaceEvenly, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Helpers.View exposing (style, whenAttr)
import Material.Icons.Types exposing (Icon)
import Ordinal
import Time.Format.I18n.I_en_us exposing (monthName)
import Types exposing (Screen)
import View.Style exposing (black, blue, rotate, sand, shadowAlt, white)


getArea : Screen -> Int
getArea scr =
    scr.width * scr.height


tallInt : Int
tallInt =
    475


formatDateTime : DateTime -> String
formatDateTime d =
    [ d |> DateTime.getDay |> Ordinal.ordinal
    , d
        |> DateTime.getMonth
        |> monthName
    , d |> DateTime.getYear |> String.fromInt
    ]
        |> String.join " "


formatDay : Date -> String
formatDay d =
    [ d |> Calendar.getDay |> Ordinal.ordinal
    , d
        |> Calendar.getMonth
        |> monthName
    , d |> Calendar.getYear |> String.fromInt
    ]
        |> String.join " "


dayParts : Date -> List String
dayParts d =
    [ d |> Calendar.getDay |> Ordinal.ordinal
    , d
        |> Calendar.getMonth
        |> monthName
    , d |> Calendar.getYear |> String.fromInt
    ]


icon : Icon msg -> Int -> Element msg
icon ic n =
    ic n Material.Icons.Types.Inherit
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


iBtn : Int -> Icon msg -> msg -> Element msg
iBtn n icn msg =
    { onPress = Just msg
    , label = icon icn n
    }
        |> Input.button
            [ Font.color black
            , shadowAlt
            , Background.color sand
            , Border.roundEach { topLeft = 0, bottomRight = 0, topRight = 15, bottomLeft = 15 }
            , padding 10
            , Element.mouseOver [ Background.color black, Font.color white ]
            ]


btn2 : Bool -> Icon msg -> String -> msg -> Element msg
btn2 inProg ic str msg =
    Input.button
        [ padding 10
        , Element.mouseOver
            [ Font.color white
            , Background.color black
            ]
        , Border.roundEach { topLeft = 0, bottomRight = 0, topRight = 25, bottomLeft = 25 }
        , Background.color sand
        , style "cursor" "wait"
            |> whenAttr inProg
        , Font.color black
        , height <| px 50
        , width <| px 120
        , shadowAlt
        , style "transition" "background-color 0.3s"
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
                ic 26 Material.Icons.Types.Inherit
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
        , Border.roundEach { topLeft = 0, bottomRight = 0, topRight = 25, bottomLeft = 25 }
        , Background.color sand
        , shadowAlt
        , style "transition" "background-color 0.3s"
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
