module View exposing (purple, textShadow, view, white, yellow)

import Calendar exposing (Day)
import Date exposing (Date)
import Day exposing (DayDict)
import Derberos.Date.Utils exposing (getNextMonth, getPrevMonth)
import Dict exposing (Dict)
import Element exposing (Attribute, Color, Element, centerX, centerY, column, el, fill, fillPortion, height, html, none, padding, paragraph, px, rgb255, row, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Helpers
import Helpers.UuidDict as UD
import Helpers.View exposing (cappedWidth, style, whenAttr)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import List.Extra exposing (greedyGroupsOf)
import List.Nonempty as Nonempty
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra exposing (unwrap)
import Set exposing (Set)
import Time exposing (Month(..), Weekday)
import Time.Format.I18n.I_en_us exposing (monthName)
import Types exposing (Emoji(..), Funnel(..), Model, Msg(..), Post, PostView(..), Route(..), Sort(..), Status(..), Tag, View(..))
import Uuid exposing (Uuid)
import Validate exposing (isValidEmail)


onCtrlEnter : msg -> Decoder msg
onCtrlEnter msg =
    Decode.map2 Tuple.pair
        (Decode.field "key" Decode.string)
        (Decode.field "ctrlKey" Decode.bool)
        |> Decode.andThen
            (\( key, ctrl ) ->
                if ctrl && key == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail ""
            )


onEnter : msg -> Decoder msg
onEnter msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Enter" then
                    Decode.succeed msg

                else
                    Decode.fail ""
            )


onKeydown : List (Decoder msg) -> Attribute msg
onKeydown decoders =
    -- Can be replaced by a Decode.oneOf when bug is fixed
    -- https://github.com/elm/html/issues/180
    Decode.value
        |> Decode.andThen
            (\val ->
                let
                    matchKeydown ds =
                        case ds of
                            decoder :: tail ->
                                val
                                    |> Decode.decodeValue decoder
                                    |> Result.map Decode.succeed
                                    |> Result.withDefault (matchKeydown tail)

                            [] ->
                                Decode.fail "No match"
                in
                matchKeydown decoders
            )
        |> Html.Events.on "keydown"
        |> Element.htmlAttribute


topRow : Element Msg
topRow =
    el [ width fill ] <|
        el [ Element.alignRight ] <|
            emoji House <|
                NavigateTo RouteHome


textShadow : Attribute msg
textShadow =
    Font.shadow { offset = ( 5, 5 ), blur = 0, color = black }


shadow : Attribute msg
shadow =
    Border.shadow
        { offset = ( 4, 4 )
        , blur = 0
        , size = 2
        , color = black
        }


title : Bool -> Element msg
title demo =
    text "FLASHBACK"
        |> el
            [ centerX
            , Font.color yellow
            , anton
            , textShadow
            , Font.size 60
            , Font.italic
            , "DEMO"
                |> text
                |> el
                    [ Font.color <| Element.rgb255 255 0 0
                    , Element.alignRight
                    , Element.rotate 25
                    , Element.moveDown 25
                    , Element.moveRight 25
                    ]
                |> when demo
                |> Element.inFront
            ]


homeButton : msg -> String -> Emoji -> Element msg
homeButton msg txt e =
    button []
        { onPress = Just msg
        , label =
            el
                [ Border.width 2
                , Border.color yellow
                , Font.color white
                , el
                    [ Border.width 1
                    , Element.moveDown 6
                    , Element.moveRight 6
                    , width fill
                    , height fill
                    ]
                    none
                    |> Element.behindContent
                , Font.size 35
                , padding 10
                , anton
                , Element.mouseOver
                    [ Font.color blue
                    ]
                ]
            <|
                row [ spacing 10 ] [ text txt, renderEmoji e ]
        }


viewMonth : Date -> DayDict (Status Post) -> Int -> Month -> Element Msg
viewMonth today posts year month =
    Day.getMonthDays year month
        |> greedyGroupsOf 4
        |> List.map
            (List.map
                (\day ->
                    button [ width fill ]
                        { onPress =
                            Just <|
                                NavigateTo <|
                                    RouteDay day
                        , label =
                            column
                                [ width <| fillPortion 1
                                , height <| px 195
                                , padding 10
                                , (if day == today then
                                    yellow

                                   else
                                    blue
                                  )
                                    |> Background.color
                                , Element.mouseOver
                                    [ Background.color violet
                                    , Font.color white
                                    ]
                                , Font.color violet
                                , source
                                , Element.pointer
                                ]
                                [ paragraph
                                    [ Font.size 30
                                    , Font.bold
                                    ]
                                    [ Element.text <|
                                        String.fromInt <|
                                            Date.day day
                                    ]
                                , (case Helpers.getStatus day posts of
                                    Loading (Just _) ->
                                        [ renderEmoji Cyclone
                                            |> el [ rotate ]
                                        , renderEmoji Paper
                                        ]

                                    Loading Nothing ->
                                        [ renderEmoji Cyclone
                                            |> el [ rotate ]
                                        ]

                                    Missing ->
                                        []

                                    Found _ ->
                                        [ renderEmoji Paper
                                        ]
                                  )
                                    |> column [ spacing 20 ]
                                    |> el
                                        [ Element.alignRight
                                        , Element.alignBottom
                                        , Font.size 40
                                        , textShadow
                                        ]
                                ]
                        }
                )
                >> Element.row [ width fill, spacing 5 ]
            )
        |> column [ width fill, spacing 5 ]


viewCalendar : Model -> Element Msg
viewCalendar model =
    let
        wd =
            px 75
    in
    [ [ Input.button [ Font.color white, Element.alignLeft ]
            { onPress = Just PrevMonth
            , label =
                Icons.chevron_left 50 Inherit
                    |> Element.html
                    |> el [ Element.moveUp 5, Font.color black ]
            }
            --|> when (previousAvailable model)
            |> el [ width fill ]
      , Input.button []
            { onPress = Nothing --Just <| SetView <| Left Months
            , label =
                [ model.month |> monthName |> text
                , model.year |> String.fromInt |> text
                ]
                    |> row
                        [ centerX
                        , spacing 10
                        , Background.color white
                        , padding 10
                        ]
            }
      , Input.button [ Font.color white, Element.alignRight ]
            { onPress = Just NextMonth
            , label =
                Icons.chevron_right 50 Inherit
                    |> Element.html
                    |> el [ Element.moveUp 5, Font.color black ]
            }
            --|> when (nextAvailable model)
            |> el [ width fill ]
      ]
        |> row [ width fill ]
    , Element.table
        [ spacing 10 ]
        { data = Calendar.weeks Time.Mon model.month model.year
        , columns =
            [ { header = weekday "Mon"
              , width = wd
              , view = .mon >> cell model
              }
            , { header = weekday "Tue"
              , width = wd
              , view = .tue >> cell model
              }
            , { header = weekday "Wed"
              , width = wd
              , view = .wed >> cell model
              }
            , { header = weekday "Thu"
              , width = wd
              , view = .thu >> cell model
              }
            , { header = weekday "Fri"
              , width = wd
              , view = .fri >> cell model
              }
            , { header = weekday "Sat"
              , width = wd
              , view = .sat >> cell model
              }
            , { header = weekday "Sun"
              , width = wd
              , view = .sun >> cell model
              }
            ]
        }
    , [ ( '◀', Element.alignLeft, -1 )
      , ( '▶', Element.alignRight, 1 )
      ]
        |> List.map
            (\( char, attr, n ) ->
                Input.button
                    [ Font.color black
                    , Font.size 40

                    --, Font.shadow
                    --{ offset = ( 4, 4 )
                    --, blur = 0
                    --, color = black
                    --}
                    , Element.mouseOver [ Font.color yellow ]
                    , Element.mouseDown
                        [ Element.moveDown 4
                        , Element.moveRight 4
                        , Font.shadow
                            { offset = ( 0, 0 )
                            , blur = 0
                            , color = black
                            }
                        ]
                    , attr
                    ]
                    { onPress =
                        --Just <|
                        --SelectDay <|
                        --Date.add Date.Days n model.current
                        Nothing
                    , label =
                        char
                            |> String.fromChar
                            |> text
                    }
             --|> when
             --(Dict.member
             --(Date.toRataDie
             --(Date.add Date.Days n model.current)
             --)
             --model.days
             --)
            )
        |> row [ width fill ]
    ]
        |> column [ spacing 10, Element.alignTop ]


cell : Model -> Day -> Element Msg
cell model day =
    let
        hv =
            model.posts
                |> Day.get day.date
                |> Maybe.andThen Helpers.extract
                |> unwrap False (always True)
    in
    Input.button
        [ Background.color grey
        , width fill
        , height <| px 75
        , style "transition" "all 0.4s"
        , Html.Attributes.class "mylad"
            |> Element.htmlAttribute
        , Html.Attributes.class "on"
            |> Element.htmlAttribute
            |> whenAttr (Just day.date == model.current)
        , Element.moveUp 5
            |> whenAttr (Just day.date == model.current)
        , Element.moveRight 5
            |> whenAttr (Just day.date == model.current)
        , Element.mouseOver
            [ Element.moveUp 5
            , Element.moveRight 5

            --[ Background.color black
            --, Font.color blue
            --, Border.shadow
            --{ offset = ( -5, 5 )
            --, blur = 0
            --, size = 0
            --, color = blue
            --}
            --, Border.color black
            ]
        , (if day.month == EQ then
            if Just day.date == model.current then
                blue

            else if hv then
                yellow

            else
                grey

           else
            Element.rgb255 190 165 140
          )
            |> Background.color
        , padding 5
        , Font.bold

        --, Border.color white
        --, Border.width 5
        ]
        { onPress =
            Just <|
                NavigateTo <|
                    RouteDay day.date
        , label =
            Date.day day.date
                |> String.fromInt
                |> text
                |> el [ Element.alignTop ]
        }


pencil : String
pencil =
    "url(data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+CjxzdmcKICAgeG1sbnM6ZGM9Imh0dHA6Ly9wdXJsLm9yZy9kYy9lbGVtZW50cy8xLjEvIgogICB4bWxuczpjYz0iaHR0cDovL2NyZWF0aXZlY29tbW9ucy5vcmcvbnMjIgogICB4bWxuczpyZGY9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkvMDIvMjItcmRmLXN5bnRheC1ucyMiCiAgIHhtbG5zOnN2Zz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciCiAgIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIKICAgaGVpZ2h0PSIyOC40IgogICB3aWR0aD0iMjguNSIKICAgaWQ9InN2ZzEyIgogICB2ZXJzaW9uPSIxLjEiCiAgIGVuYWJsZS1iYWNrZ3JvdW5kPSJuZXcgMCAwIDUwIDUwIgogICB2aWV3Qm94PSIwIDAgMjguNSAyOC40Ij4KICA8bWV0YWRhdGEKICAgICBpZD0ibWV0YWRhdGExOCI+CiAgICA8cmRmOlJERj4KICAgICAgPGNjOldvcmsKICAgICAgICAgcmRmOmFib3V0PSIiPgogICAgICAgIDxkYzpmb3JtYXQ+aW1hZ2Uvc3ZnK3htbDwvZGM6Zm9ybWF0PgogICAgICAgIDxkYzp0eXBlCiAgICAgICAgICAgcmRmOnJlc291cmNlPSJodHRwOi8vcHVybC5vcmcvZGMvZGNtaXR5cGUvU3RpbGxJbWFnZSIgLz4KICAgICAgICA8ZGM6dGl0bGU+PC9kYzp0aXRsZT4KICAgICAgPC9jYzpXb3JrPgogICAgPC9yZGY6UkRGPgogIDwvbWV0YWRhdGE+CiAgPGRlZnMKICAgICBpZD0iZGVmczE2IiAvPgogIDxwYXRoCiAgICAgaWQ9InBhdGgyIgogICAgIGQ9Ik0gMCwyOC40IDIuNSwxOC41IDE3LjQsMy42IDI0LjgsMTEgOS45LDI1LjkgWiBNIDQuMywxOS41IDIuOCwyNS42IDguOSwyNC4xIDIyLDExIDE3LjQsNi40IFoiIC8+CiAgPHBhdGgKICAgICBpZD0icGF0aDQiCiAgICAgZD0iTSA4LjIsMjUuMyBDIDcuNiwyMi44IDUuNiwyMC44IDMuMSwyMC4yIGwgMC41LC0xLjkgYyAzLjIsMC44IDUuNywzLjMgNi41LDYuNSB6IiAvPgogIDxwYXRoCiAgICAgaWQ9InBhdGg2IgogICAgIGQ9Ik0gMTkuNjk4LDcuMjg3IDIxLjExMiw4LjcwMSA4LjEwMiwyMS43MjEgNi42ODgsMjAuMzA5IFoiIC8+CiAgPHBhdGgKICAgICBpZD0icGF0aDgiCiAgICAgZD0iTSAxLjQsMjcgNC4zLDI2LjMgQyA0LDI1LjIgMy4yLDI0LjQgMi4xLDI0LjEgWiIgLz4KICA8cGF0aAogICAgIGlkPSJwYXRoMTAiCiAgICAgZD0iTSAyNS40LDEwLjQgMTgsMyAyMSwwIDIxLjUsMC4xIGMgMy42LDAuNSA2LjQsMy4zIDYuOSw2LjkgbCAwLjEsMC41IHogTSAyMC44LDMgMjUuNCw3LjYgMjYuMyw2LjcgQyAyNS44LDQuNCAyNCwyLjYgMjEuNywyLjEgWiIgLz4KPC9zdmc+Cg==), auto"


weekday : String -> Element msg
weekday =
    text >> el [ Font.bold ]


press : Attribute msg
press =
    Element.mouseDown
        [ Element.moveDown 5
        , Element.moveRight 5
        , Font.shadow
            { offset = ( 0, 0 )
            , blur = 0
            , color = black
            }
        ]


emoji : Emoji -> msg -> Element msg
emoji e msg =
    button
        [ Font.size 40
        , textShadow
        ]
        { onPress = Just msg
        , label = el [ press ] <| renderEmoji e
        }


renderEmoji : Emoji -> Element msg
renderEmoji e =
    (case e of
        Disk ->
            "💾"

        Next ->
            "⏩"

        Previous ->
            "⏪"

        New ->
            "🆕"

        House ->
            "🏠"

        Edit ->
            "📝"

        Trash ->
            "🗑"

        TagEmoji ->
            "🏷"

        Tick ->
            "✅"

        X ->
            "❌"

        Key ->
            "🔑"

        Sun ->
            "🌞"

        Chart ->
            "📋"

        Calendar ->
            "🗓"

        Dice ->
            "🎲"

        Cogs ->
            "⚙️"

        Bang ->
            "💥"

        Clock ->
            "🕰"

        Cyclone ->
            "🌀"

        Abc ->
            "🔤"

        Sherlock ->
            "🔍"

        LeftArrow ->
            "⬅️"

        Paper ->
            "📄"

        Locked ->
            "🔒"

        Unlocked ->
            "🔓"
    )
        |> text


medium : String -> Element msg
medium =
    text
        >> el
            [ Font.color yellow
            , anton
            , textShadow
            , Font.size 30
            , Font.italic
            ]


source : Attribute msg
source =
    Font.family
        [ Font.typeface "Source Sans Pro"
        , Font.sansSerif
        ]


anton : Attribute msg
anton =
    Font.family
        [ Font.typeface "Burbank"
        , Font.sansSerif
        ]


blue : Color
blue =
    rgb255 112 237 204


purple : Color
purple =
    rgb255 145 130 254


violet : Color
violet =
    rgb255 92 39 81


white : Color
white =
    rgb255 255 255 255


yellow : Color
yellow =
    rgb255 255 238 147


black : Color
black =
    rgb255 0 0 0


grey : Color
grey =
    rgb255 255 245 235


editor : String -> Msg -> Element Msg
editor txt msg =
    Html.textarea
        [ Html.Attributes.id "editor"
        , Html.Attributes.value txt
        , Html.Events.onInput BodyUpdate
        ]
        []
        |> Element.html
        |> el
            [ width fill
            , height fill
            , source
            , Font.size 20
            , shadow
            , padding 10
            , Background.color <| rgb255 255 250 205
            , onKeydown [ onCtrlEnter msg ]
            ]


getSort : Sort -> (List Tag -> List Tag)
getSort s =
    case s of
        Alpha reversed ->
            List.sortBy .name
                >> (if reversed then
                        List.reverse

                    else
                        identity
                   )


bar : Date -> Element Msg
bar day =
    row [ spaceEvenly, width fill ]
        [ emoji Previous <| NavigateTo <| RouteDay <| Day.shift -1 <| day
        , day
            |> formatDay
            |> text
            |> List.singleton
            |> paragraph
                [ Font.color yellow
                , anton
                , textShadow
                , Font.size 30
                , Font.italic
                ]
        , emoji Chart <| NavigateTo <| RouteWeek day
        , emoji Calendar <|
            NavigateTo <|
                RouteMonth
                    (Date.month day)
                    (Date.year day)
        , emoji Next <| NavigateTo <| RouteDay <| Day.shift 1 <| day
        ]


viewPostCreate : String -> List Uuid -> Bool -> Date -> Element Msg
viewPostCreate postEditorBody postCreateTags loading day =
    [ topRow
    , bar day
    , editor postEditorBody <| PostCreateSubmit day
    , if loading then
        renderEmoji Cyclone
            |> el [ rotate, centerX ]

      else
        row [ spacing 40, centerX ]
            [ emoji Tick <| PostCreateSubmit day
            , row [ spacing 5 ]
                [ emoji TagEmoji <| PostViewSet PostTags
                , medium
                    ("["
                        ++ String.fromInt
                            (List.length postCreateTags)
                        ++ "]"
                    )
                ]
            , emoji X <| NavigateTo RouteHome
            ]
    ]
        |> column
            [ centerX
            , spacing 20
            , height fill
            , cappedWidth 550
            ]


viewPostStatic : Date -> String -> List Uuid -> Element Msg
viewPostStatic day body tags =
    [ topRow
    , bar day
    , body
        -- HACK: Need to pass through Html in order
        -- to preserve formatting.
        |> Html.text
        |> List.singleton
        |> Html.div
            [ Html.Attributes.style "white-space" "pre-wrap"
            , Html.Attributes.style "height" "0px"
            ]
        |> Element.html
        |> el
            [ width fill
            , fill |> Element.minimum 150 |> height
            , source
            , padding 20
            , shadow
            , Background.color yellow
            , Element.scrollbarY
            ]
    , [ emoji Edit <| PostUpdateStart body
      , row [ spacing 5 ]
            [ emoji TagEmoji <| PostViewSet PostTags
            , medium
                ("["
                    ++ String.fromInt
                        (List.length tags)
                    ++ "]"
                )
            ]
      ]
        |> row [ spacing 40, centerX ]
    ]
        |> column
            [ centerX
            , spacing 20
            , height fill
            , cappedWidth 550
            ]


viewPostTags : Sort -> String -> UD.UuidDict Tag -> List Uuid -> (Tag -> Msg) -> Element Msg
viewPostTags tagSort tagCreateName tags ts toggle =
    column
        [ spacing 30
        , width fill
        ]
        [ topRow
        , row [ spacing 10, centerX ]
            [ case tagSort of
                Alpha reversed ->
                    row [ spacing 5 ]
                        [ emoji Abc <| TagSortUpdate <| Alpha <| not reversed
                        , (if reversed then
                            "↑"

                           else
                            "↓"
                          )
                            |> text
                            |> el [ Font.size 40 ]
                        ]
            ]
        , viewNewTagRow tagCreateName
        , tags
            |> UD.values
            |> getSort tagSort
            |> List.map
                (\t ->
                    row
                        [ spaceEvenly, width fill ]
                        [ header t.name <| NavigateTo <| RouteTagPosts t.id
                        , emoji
                            (if List.member t.id ts then
                                Tick

                             else
                                X
                            )
                            (toggle t)
                        ]
                )
            |> column [ spacing 20, width fill ]
        , el [ centerX ] <| emoji LeftArrow <| PostViewSet PostView
        ]


viewPostUpdate : String -> Bool -> Post -> Element Msg
viewPostUpdate postEditorBody loading post =
    [ topRow
    , post.date
        |> formatDay
        |> text
        |> el
            [ centerX
            , Font.color yellow
            , anton
            , textShadow
            , Font.size 40
            , Font.italic
            ]
    , PostUpdateSubmit post.id
        |> editor postEditorBody
    , if loading then
        renderEmoji Cyclone
            |> el [ rotate, centerX ]

      else
        [ emoji Tick <| PostUpdateSubmit post.id
        , emoji X PostUpdateCancel
        ]
            |> row [ spacing 40, centerX ]
    ]
        |> column
            [ spacing 20
            , height fill
            , cappedWidth 550
            , centerX
            ]


view : Model -> Html Msg
view model =
    let
        isMobile =
            model.screen.width < 450
    in
    [ none
        |> el
            [ width fill
            , height <| px 10
            , Background.color black
            ]
    , if model.force then
        viewHome model

      else
        case model.view of
            ViewHome ->
                viewPage model

            ViewSettings ->
                model.auth
                    |> unwrap
                        ([ text "You're a guest, you don't have settings!"
                         , viewBuy
                         ]
                            |> column [ spacing 20, centerX ]
                        )
                        (\auth ->
                            [ [ text "Email: "
                              , text auth.email
                              ]
                                |> row [ spacing 10 ]
                            , Input.button []
                                { onPress = Just Logout
                                , label = text "LOGOUT"
                                }
                            ]
                                |> column [ spacing 20 ]
                        )
                    |> el
                        [ centerX
                        ]
                    |> viewFrame model

            ViewTags ->
                [ [ [ Input.text
                        [ Border.rounded 0
                        , Border.width 0
                        , width fill
                        , padding 10

                        --, style "cursor" "wait"
                        --|> whenAttr model.inProgress
                        , Html.Attributes.disabled (model.funnel /= Types.Hello)
                            |> Element.htmlAttribute
                        , (if model.funnel /= Types.Hello then
                            grey

                           else
                            white
                          )
                            |> Background.color

                        --, onEnter Submit
                        --|> whenAttr valid
                        ]
                        { onChange = TagCreateNameUpdate
                        , label = Input.labelHidden ""
                        , placeholder =
                            Just <|
                                Input.placeholder
                                    [--rale
                                    ]
                                <|
                                    text "New tag"
                        , text = model.tagCreateName
                        }
                    , Input.button [ centerY, padding 15, Border.width 1 ]
                        { onPress = Just TagCreateSubmit
                        , label = text "Submit"
                        }
                    ]
                        |> row []
                  , model.tags
                        |> UD.values
                        |> List.map
                            (\t ->
                                model.tagsBeingEdited
                                    |> UD.get t.id
                                    |> unwrap
                                        ([ Input.button []
                                            { onPress = Just <| TagSelect t.id
                                            , label = text t.name
                                            }

                                         --(t.name ++ " [" ++  ++ "]")
                                         --<|
                                         --NavigateTo <|
                                         --RouteTagPosts t.id
                                         , text <| String.fromInt t.count
                                         , row []
                                            [ emoji Edit <| TagUpdate t <| Just t.name
                                            , emoji Trash <| TagDelete t
                                            ]
                                         ]
                                            |> row [ spacing 20, width fill ]
                                        )
                                        (\str ->
                                            Input.text
                                                [ Border.rounded 0, shadow ]
                                                { label =
                                                    Input.labelRight [] <|
                                                        row []
                                                            [ emoji Tick <| TagUpdateSubmit { t | name = str }
                                                            , emoji X <| TagUpdate t Nothing
                                                            ]
                                                , onChange = Just >> TagUpdate t
                                                , placeholder = Nothing
                                                , text = str
                                                }
                                        )
                            )
                        |> column []
                  ]
                    |> column [ cappedWidth 450, centerX ]
                , model.tag
                    |> whenJust
                        (\t ->
                            model.posts
                                |> Day.values
                                |> List.filterMap Helpers.extract
                                |> List.filter
                                    (\p ->
                                        List.member t p.tags
                                    )
                                |> List.map (.date >> Date.toIsoString >> text)
                                |> column [ spacing 10 ]
                        )
                ]
                    |> row [ centerX, spacing 50 ]
                    |> viewFrame model

            ViewLogin ->
                let
                    login =
                        Login model.loginForm.email model.loginForm.password
                in
                [ Input.email
                    [ Border.rounded 0
                    , shadow
                    , onKeydown [ onEnter login ]
                    ]
                    { label = Input.labelHidden ""
                    , onChange = LoginFormEmailUpdate
                    , placeholder = Just <| Input.placeholder [] <| text "email"
                    , text = model.loginForm.email
                    }
                , Input.currentPassword
                    [ Border.rounded 0
                    , shadow
                    , onKeydown [ onEnter login ]
                    ]
                    { label =
                        Input.button [ centerY, padding 15, Font.size 30 ]
                            { onPress = Just LoginFormPasswordVisibleToggle
                            , label =
                                renderEmoji
                                    (if model.loginForm.passwordVisible then
                                        Unlocked

                                     else
                                        Locked
                                    )
                            }
                            |> Input.labelRight []
                    , onChange = LoginFormPasswordUpdate
                    , placeholder = Just <| Input.placeholder [] <| text "password"
                    , text = model.loginForm.password
                    , show = model.loginForm.passwordVisible
                    }
                , model.errors
                    |> List.map (\str -> text <| "- " ++ str)
                    |> column [ spacing 3 ]
                , homeButton login "LOGIN" Key
                , homeButton
                    (Signup model.loginForm.email
                        model.loginForm.password
                    )
                    "SIGNUP"
                    New
                ]
                    |> column [ centerX, spacing 20 ]
                    |> viewFrame model

            _ ->
                text "my sweer lad"
                    |> el [ centerX, centerY ]
    ]
        |> column [ spacing 20, height fill, width fill ]
        |> Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Nothing
                    , backgroundColor = Nothing
                    , shadow = Nothing
                    }
                ]
                    |> (if isMobile then
                            (::) Element.noHover

                        else
                            identity
                       )
            }
            ([ height fill
             , width fill
             ]
                |> (++)
                    (if isMobile then
                        removeTapColor :: disableUserSelect

                     else
                        []
                    )
            )


viewHome : Model -> Element Msg
viewHome model =
    [ [ ( "Hello", RouteSettings )
      , ( "Alternatives", RouteSettings )
      , ( "Pricing", RouteSettings )
      , ( "Contact", RouteSettings )
      ]
        |> List.map (\( name, r ) -> text name)
        |> column
            [ spacing 20
            , Element.alignLeft
            , Element.alignTop
            , padding 50
            , Font.size 50
            ]
    , [ text "BLOCKOUT"
            |> el
                [ Font.family
                    [ Font.typeface "Varela"
                    ]
                , Font.semiBold
                , Font.size 75
                , style "animation-name" "fadeIn"
                , style "animation-duration" "1s"
                ]
      , text "The world's first private journal."
            |> el [ Font.italic, Font.size 35 ]
      , viewFunnel model
      ]
        |> column
            [ spacing 20
            , padding 50
            , Element.alignTop
            , Element.alignRight
            ]
    ]
        |> row [ spacing 20, height fill, width fill ]


viewFunnel : Model -> Element Msg
viewFunnel model =
    if model.auth == Nothing then
        [ [ Input.email
                [ Border.rounded 0
                , Border.width 0
                , width fill
                , padding 10

                --, style "cursor" "wait"
                --|> whenAttr model.inProgress
                , Html.Attributes.disabled (model.funnel /= Types.Hello)
                    |> Element.htmlAttribute
                , (if model.funnel /= Types.Hello then
                    grey

                   else
                    white
                  )
                    |> Background.color

                --, onEnter Submit
                --|> whenAttr valid
                ]
                { onChange = LoginFormEmailUpdate
                , label = Input.labelHidden ""
                , placeholder =
                    Just <|
                        Input.placeholder
                            [--rale
                            ]
                        <|
                            text "Your email address"
                , text = model.loginForm.email
                }
          , el [ height fill, width <| px 1, Background.color black ] none
          , Input.button
                [ padding 15

                --, Background.color darkBlue
                --, Font.color white
                --, style "cursor" "wait"
                --|> whenAttr model.inProgress
                , style "cursor" "not-allowed"
                    |> whenAttr
                        (not <| isValidEmail model.loginForm.email)
                , style "transition" "all 0.2s"
                , Element.mouseOver
                    [ Font.color white
                    , Background.color black
                    ]
                    |> whenAttr
                        (isValidEmail model.loginForm.email)
                ]
                { onPress =
                    (if model.funnel == Hello then
                        EmailSubmit

                     else
                        Change
                    )
                        |> Just
                , label =
                    text <|
                        if model.funnel /= Types.Hello then
                            "Change"

                        else
                            "Continue"
                }
          ]
            |> row
                [ cappedWidth 600
                , Element.alignRight

                --, style "cursor" "wait"
                --|> whenAttr model.inProgress
                --, (if model.inProgress then
                --grey
                --else
                --white
                --)
                --|> Background.color
                --, padding 10
                , Border.width 1
                ]
        , case model.funnel of
            Hello ->
                Input.button
                    [ Font.underline ]
                    { onPress = Just Force
                    , label = text "Or give the demo a try"
                    }

            WelcomeBack nonce ->
                [ text "Welcome back"
                    |> el [ Font.italic ]
                , [ Input.currentPassword
                        [ Border.rounded 0
                        , Border.width 0
                        , width fill
                        , padding 10
                        ]
                        { onChange = LoginFormPasswordUpdate
                        , label = Input.labelHidden ""
                        , show = False
                        , placeholder =
                            text "Your password"
                                |> Input.placeholder []
                                |> Just
                        , text = model.loginForm.password
                        }
                  , el [ height fill, width <| px 1, Background.color black ] none
                  , Input.button
                        [ padding 15
                        , style "transition" "all 0.2s"
                        , Element.mouseOver
                            [ Font.color white
                            , Background.color black
                            ]
                        ]
                        { onPress = Just <| LoginSubmit nonce
                        , label = text "Submit"
                        }
                  ]
                    |> row
                        [ width fill
                        , Border.width 1
                        ]
                ]
                    |> column
                        [ width fill
                        , spacing 20
                        ]

            JoinUs ->
                [ text "Please provide a password in order to sign up"
                    |> el [ Font.italic ]
                , [ Input.currentPassword
                        [ Border.rounded 0
                        , Border.width 1
                        , Border.color black
                        , width fill
                        , padding 10
                        ]
                        { onChange = LoginFormPasswordUpdate
                        , label = Input.labelHidden ""
                        , show = False
                        , placeholder =
                            text "Your password"
                                |> Input.placeholder []
                                |> Just
                        , text = model.loginForm.password
                        }
                  , Input.text
                        [ Border.rounded 0
                        , Border.width 1
                        , Border.color black
                        , width fill
                        , padding 10
                        ]
                        { onChange = LoginFormPasswordUpdate
                        , label = Input.labelHidden ""
                        , placeholder =
                            text "Optional password hint"
                                |> Input.placeholder []
                                |> Just
                        , text = ""
                        }
                  , Input.button
                        [ padding 10
                        , Element.alignRight
                        , style "transition" "all 0.2s"
                        , Element.mouseOver
                            [ Font.color white
                            , Background.color black
                            ]
                        , Border.width 1
                        ]
                        { onPress = Just SignupSubmit
                        , label = text "Submit"
                        }
                  ]
                    |> column
                        [ width fill
                        ]
                ]
                    |> column
                        [ width fill
                        , spacing 20
                        ]
        ]
            |> column [ spacing 20 ]

    else
        Input.button
            [ padding 15
            , style "transition" "all 0.2s"
            , Element.mouseOver
                [ Font.color white
                , Background.color black
                ]
            , Border.width 1
            ]
            { onPress = Just Force
            , label = text "Return to app"
            }


viewFrame : Model -> Element Msg -> Element Msg
viewFrame model elem =
    [ [ [ Input.button
            [ Font.family
                [ Font.typeface "Varela"
                ]
            , Font.semiBold
            , Font.size 25
            ]
            { onPress = Just Force
            , label = text "BLOCKOUT"
            }
        , text "DEMO"
            |> el [ Font.light ]
            |> when (model.auth == Nothing)
        ]
            |> row [ spacing 10 ]
      , [ ( "Calendar", RouteHome, ViewHome )
        , ( "Tags", RouteTags, ViewTags )
        , ( "Settings", RouteSettings, ViewSettings )
        ]
            |> List.map
                (\( n, r, v ) ->
                    Input.button
                        [ Font.underline |> whenAttr (v == model.view) ]
                        { onPress = Just <| NavigateTo r
                        , label = text n
                        }
                )
            |> row [ spacing 40 ]

      --, Input.button []
      --{ onPress = Just (NavigateTo RouteSettings)
      --, label =
      --Icons.face 30 Inherit
      --|> Element.html
      --}
      ]
        |> row [ width fill, spaceEvenly, padding 20 ]
    , elem
    ]
        |> column [ spacing 20, height fill, cappedWidth 1450, centerX ]


viewBuy : Element Msg
viewBuy =
    [ text "Pay Now"
    ]
        |> column []


viewPage : Model -> Element Msg
viewPage model =
    [ viewCalendar model
    , viewPost_ model
    ]
        |> row [ width fill, height fill, spacing 20, padding 20 ]
        |> viewFrame model


placeholder : Element Msg
placeholder =
    Input.button
        [ centerX
        , centerY
        , Font.family
            [ Font.typeface "Times New Roman"
            ]
        , Font.size 30
        , Font.italic
        ]
        { onPress =
            RouteToday
                |> NavigateTo
                |> Just
        , label = text "to begin..."
        }


viewPost_ : Model -> Element Msg
viewPost_ model =
    model.current
        |> unwrap
            placeholder
            (\d ->
                let
                    fn fn1 =
                        Html.textarea
                            [ Html.Attributes.id "editor"
                            , Html.Attributes.value model.postEditorBody
                            , Html.Attributes.style "font-size" "inherit"
                            , Html.Attributes.style "font-family" "inherit"
                            , Html.Attributes.style "cursor" "inherit"
                            , Html.Attributes.style "line-height" "40px"
                            , Html.Attributes.style "padding" "0px"
                            , Html.Events.onInput BodyUpdate
                            ]
                            []
                            |> Element.html
                            |> el
                                [ width fill
                                , style "cursor" "text"

                                --, height fill
                                , Element.alignTop
                                , height <| px 750
                                , Background.color grey
                                , Font.size 35
                                , padding 20
                                , onKeydown [ onCtrlEnter fn1 ]
                                , Font.family
                                    [ Font.typeface "EB Garamond"
                                    ]
                                ]

                    data =
                        model.posts
                            |> Helpers.getStatus d

                    create =
                        fn (PostCreateSubmit d)

                    make post =
                        if model.postBeingEdited then
                            fn (PostUpdateSubmit post.id)

                        else
                            Input.button
                                [ width fill
                                , height <| px 750
                                , Element.mouseOver [ Background.color grey ]
                                , padding 20
                                , Font.size 35
                                , Element.alignTop
                                , Font.family
                                    [ Font.typeface "EB Garamond"
                                    ]
                                , style "cursor" pencil
                                ]
                                { onPress = Just <| PostUpdateStart post.body
                                , label =
                                    post.body
                                        -- HACK: Need to pass through Html in order
                                        -- to preserve formatting.
                                        |> Html.text
                                        |> List.singleton
                                        |> Html.div
                                            [ Html.Attributes.style "white-space" "pre-wrap"
                                            , Html.Attributes.style "line-height" "40px"
                                            , Html.Attributes.style "height" "100%"
                                            , Html.Attributes.style "width" "100%"
                                            , Html.Attributes.style "overflow-y" "auto"
                                            , Html.Attributes.style "word-break" "break-word"
                                            ]
                                        |> Element.html
                                }
                in
                [ model.posts
                    |> Day.get d
                    |> Maybe.andThen Helpers.extract
                    |> whenJust
                        (\p ->
                            model.tags
                                |> UD.values
                                |> List.map
                                    (\t ->
                                        Input.button
                                            [ padding 10
                                            , Border.width 1
                                            , (if List.member t.id p.tags then
                                                blue

                                               else
                                                white
                                              )
                                                |> Background.color
                                            ]
                                            { onPress = Just <| PostTagToggle p t
                                            , label = text t.name
                                            }
                                    )
                                |> Element.wrappedRow [ spacing 20 ]
                        )
                , case data of
                    Missing ->
                        create

                    Loading ma ->
                        ma
                            |> unwrap create make

                    Found a ->
                        make a
                ]
                    |> column [ height fill, width fill ]
            )


viewPost : Date -> Model -> Element Msg
viewPost day model =
    case model.postView of
        PostView ->
            model.posts
                |> Helpers.getStatus day
                |> (\data ->
                        let
                            update =
                                viewPostUpdate model.postEditorBody model.postSaveInProgress

                            create =
                                viewPostCreate model.postEditorBody model.postCreateTags model.postSaveInProgress day

                            make post =
                                if model.postBeingEdited then
                                    update post

                                else
                                    viewPostStatic post.date post.body post.tags
                        in
                        case data of
                            Missing ->
                                create

                            Loading ma ->
                                ma
                                    |> unwrap create make

                            Found a ->
                                make a
                   )

        PostTags ->
            model.posts
                |> Day.get day
                |> Maybe.andThen Helpers.extract
                |> unwrap
                    (viewPostTags model.tagSort
                        model.tagCreateName
                        model.tags
                        --model.postCreateTags
                        []
                        PostCreateTagToggle
                    )
                    (\post ->
                        viewPostTags model.tagSort model.tagCreateName model.tags post.tags (PostTagToggle post)
                    )


disableUserSelect : List (Attribute msg)
disableUserSelect =
    [ "", "-ms-", "-moz-", "-webkit-" ]
        |> List.map
            (\prefix ->
                style (prefix ++ "user-select") "none"
            )


removeTapColor : Attribute msg
removeTapColor =
    style "-webkit-tap-highlight-color" "transparent"


viewNewTagRow : String -> Element Msg
viewNewTagRow str =
    row [ spacing 10, centerX ]
        [ Input.text
            [ Border.rounded 0
            , shadow
            , onKeydown [ onEnter TagCreateSubmit ]
            ]
            { label = Input.labelHidden ""
            , onChange = TagCreateNameUpdate
            , placeholder = Nothing
            , text = str
            }
        , emoji New TagCreateSubmit
        ]


viewDay : DayDict (Status Post) -> Date -> Date -> Element Msg
viewDay posts today d =
    button
        [ Border.width 2
        , Border.color yellow
        , Font.color white
        , el
            [ Border.width 1
            , Element.moveDown 6
            , Element.moveRight 6
            , width fill
            , height fill
            ]
            none
            |> Element.behindContent
        , Font.size 35
        , padding 10
        , anton
        , Element.mouseOver
            [ Font.color blue
            ]
        , width fill
        ]
        { onPress =
            d
                |> RouteDay
                |> NavigateTo
                |> Just
        , label =
            row [ spaceEvenly, width fill ]
                [ row [ spacing 10 ]
                    [ text
                        ((String.fromInt <|
                            Date.day d
                         )
                            ++ " "
                            ++ (fromWeekday <| Date.weekday d)
                        )
                    , renderEmoji LeftArrow
                        |> when (d == today)
                    ]
                , (case Helpers.getStatus d posts of
                    Loading (Just _) ->
                        [ renderEmoji Cyclone
                            |> el [ rotate ]
                        , renderEmoji Tick
                        ]

                    Loading Nothing ->
                        [ renderEmoji Cyclone
                            |> el [ rotate ]
                        ]

                    Missing ->
                        [ renderEmoji X
                        ]

                    Found _ ->
                        [ renderEmoji Tick
                        ]
                  )
                    |> row [ spacing 5 ]
                ]
        }


goToPreviousMonth : Int -> Month -> Msg
goToPreviousMonth year month =
    (if month == Jan then
        RouteMonth Dec (year - 1)

     else
        RouteMonth (getPrevMonth month) year
    )
        |> NavigateTo


goToNextMonth : Int -> Month -> Msg
goToNextMonth year month =
    (if month == Dec then
        RouteMonth Jan (year + 1)

     else
        RouteMonth (getNextMonth month) year
    )
        |> NavigateTo


header : String -> msg -> Element msg
header str msg =
    button
        [ Font.color yellow
        , anton
        , textShadow
        , Font.size 30
        , Font.italic
        , Element.mouseOver [ Font.color blue ]
        ]
        { onPress = Just msg
        , label = el [ press ] <| text str
        }


formatDay : Date -> String
formatDay d =
    [ d |> Date.day |> String.fromInt |> String.padLeft 2 '0'
    , d
        |> Date.month
        |> monthToString
        |> String.toUpper
    , d |> Date.year |> String.fromInt
    ]
        |> String.join " "


whenJust : (a -> Element msg) -> Maybe a -> Element msg
whenJust =
    unwrap none


when : Bool -> Element msg -> Element msg
when b elem =
    if b then
        elem

    else
        none


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


fromWeekday : Weekday -> String
fromWeekday day =
    case day of
        Time.Mon ->
            "MON"

        Time.Tue ->
            "TUE"

        Time.Wed ->
            "WED"

        Time.Thu ->
            "THU"

        Time.Fri ->
            "FRI"

        Time.Sat ->
            "SAT"

        Time.Sun ->
            "SUN"


rotate : Attribute msg
rotate =
    style
        "animation"
        "rotation 2s infinite linear"
