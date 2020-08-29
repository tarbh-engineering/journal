module View exposing (view)

import Calendar exposing (Day)
import CustomScalars exposing (Uuid)
import Date exposing (Date)
import Day
import Element exposing (Attribute, Color, Element, alignBottom, centerX, centerY, column, el, fill, height, html, none, padding, paddingXY, paragraph, px, rgb255, row, scrollbarY, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input exposing (button)
import Helpers
import Helpers.UuidDict as UD
import Helpers.View exposing (cappedWidth, style, when, whenAttr, whenJust)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Icon
import Json.Decode as Decode exposing (Decoder)
import Material.Icons as Icons
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Time exposing (Month(..))
import Time.Format.I18n.I_en_us exposing (monthName)
import Types exposing (Def(..), Funnel(..), Model, Msg(..), Post, Route(..), Sort(..), Status(..), Tag, View(..))
import Validate exposing (isValidEmail)
import View.Img
import View.Misc exposing (btn, btn2, btn3, formatDay, icon, isWide, lnk, spinner)
import View.Style exposing (abel, black, blue, ebg, fadeIn, grey, rotate, varela, white, yellow)


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


shadow2 =
    Border.shadow
        { offset = ( 3, 3 )
        , blur = 4
        , size = 2
        , color = Element.rgb255 150 150 150
        }


shadow : Attribute msg
shadow =
    Border.shadow
        { offset = ( 4, 4 )
        , blur = 0
        , size = 2
        , color = black
        }


viewCalendar : Model -> Element Msg
viewCalendar model =
    let
        ht =
            if model.screen.height < 700 then
                40

            else if isWide model.screen then
                60

            else if model.screen.width > 700 then
                80

            else
                50

        wd =
            if model.screen.width < 450 then
                fill

            else
                px ht
    in
    [ [ { onPress = Just PrevMonth
        , label =
            icon Icons.chevron_left 30
                |> el [ centerX, centerY ]
        }
            |> rBtn
      , [ model.month |> monthName |> text
        , model.year |> String.fromInt |> text
        ]
            |> row
                [ centerX
                , spacing 10
                , Background.color white
                , padding 10
                ]
      , { onPress = Just NextMonth
        , label =
            icon Icons.chevron_right 30
                |> el [ centerX, centerY ]
        }
            |> rBtn
      ]
        |> row [ width fill, spaceEvenly, padding 5 ]
    , Element.table
        [ spacing 5 ]
        { data = Calendar.weeks Time.Mon model.month model.year
        , columns =
            [ { header = weekday "Mon"
              , width = wd
              , view = .mon >> viewCell model ht
              }
            , { header = weekday "Tue"
              , width = wd
              , view = .tue >> viewCell model ht
              }
            , { header = weekday "Wed"
              , width = wd
              , view = .wed >> viewCell model ht
              }
            , { header = weekday "Thu"
              , width = wd
              , view = .thu >> viewCell model ht
              }
            , { header = weekday "Fri"
              , width = wd
              , view = .fri >> viewCell model ht
              }
            , { header = weekday "Sat"
              , width = wd
              , view = .sat >> viewCell model ht
              }
            , { header = weekday "Sun"
              , width = wd
              , view = .sun >> viewCell model ht
              }
            ]
        }
    ]
        |> column
            [ spacing 10
            , width fill
                |> whenAttr (model.screen.width < 450)
            , centerX
            ]


cell2 : Model -> Day -> Element Msg
cell2 model day =
    let
        hv =
            model.posts
                |> Day.get day.date
                |> unwrap False (always True)

        curr =
            Just day.date == model.current && day.month == EQ
    in
    Input.button
        [ Background.color grey
        , width fill
        , height <| px 50
        , style "transition" "all 0.4s"
        , shiftShadow
            |> whenAttr curr
        , Element.moveUp 5
            |> whenAttr curr
        , Element.moveRight 5
            |> whenAttr curr
        , (if day.month == EQ then
            if curr then
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


viewCell : Model -> Int -> Day -> Element Msg
viewCell model n day =
    let
        pst =
            model.posts
                |> Day.get day.date

        hv =
            pst
                |> unwrap False (always True)

        curr =
            Just day.date == model.current
    in
    Input.button
        [ height <| px n

        --, style "transition" "all 0.8s"
        --, Html.Attributes.class "shift"
        --|> Element.htmlAttribute
        --|> whenAttr (not model.isMobile)
        --, shiftShadow
        --|> whenAttr (Just day.date == model.current)
        --, Element.moveUp 5
        --|> whenAttr (Just day.date == model.current)
        --, Element.moveRight 5
        --|> whenAttr (Just day.date == model.current)
        , Element.mouseOver
            [ Border.color black

            --, Element.moveUp 5
            --, Element.moveRight 5
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
            |> whenAttr (not model.isMobile)
        , (if day.month == EQ then
            if hv then
                yellow

            else
                grey

           else
            Element.rgb255 190 165 140
          )
            |> Background.color
        , none
            |> el
                [ width fill
                , height fill
                , Background.color blue
                , style "transform-origin" "center"
                , style "animation" "enter 0.3s"
                    |> whenAttr (not model.postBeingEdited)
                ]
            |> Element.inFront
            |> whenAttr curr
        , [ Date.day day.date
                |> String.fromInt
                |> String.padLeft 2 '0'
                |> text
                |> el
                    [ Element.alignTop
                    , Element.alignLeft
                    , Font.bold
                    , Font.size (n // 2)
                    , abel
                    ]
          , [ icon Icons.label 15
                |> when (pst |> unwrap False (\p -> not <| List.isEmpty p.tags))
            , icon Icons.edit 15
                |> when (pst |> unwrap False (\p -> isJust p.body))
            ]
                |> row [ spacing 10 ]
          ]
            |> column [ height fill, width fill ]
            |> Element.inFront
        ]
        { onPress =
            (if curr then
                if pst |> unwrap True (.body >> isNothing) then
                    PostUpdateStart

                else
                    PostViewToggle

             else
                RouteDay day.date
                    |> NavigateTo
            )
                |> Just
        , label = none
        }


weekday : String -> Element msg
weekday =
    text >> el [ Font.bold ]


view : Model -> Html Msg
view model =
    let
        wide =
            isWide model.screen

        frame =
            if wide then
                viewFrame model

            else
                viewFrameMobile model

        wd =
            if model.screen.height <= 768 then
                10

            else
                20
    in
    [ none
        |> el
            [ width fill
            , height <| px 10
            , Background.color black
            ]
        |> when (not model.isMobile)
    , case model.view of
        ViewHome ->
            if wide then
                viewHome model

            else
                viewHomeMobile model

        ViewMagic ->
            model.magic
                |> whenJust
                    (\b ->
                        if b then
                            [ text "Choose a password"
                                |> el [ Font.italic ]
                            , Input.currentPassword
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
                            , text "Don't forget it or you are screwed."
                                |> el [ Font.italic, Font.size 15 ]
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
                            , btn "Submit" <| SignupSubmit
                            ]
                                |> column
                                    [ cappedWidth 450
                                    , spacing 30
                                    , centerX
                                    , padding 40
                                    ]

                        else
                            [ text "This link is broken."
                            , btn "Re-send email" (NavigateTo RouteHome)
                                |> el [ centerX ]
                            ]
                                |> column
                                    [ centerX
                                    , spacing 20
                                    ]
                    )

        ViewCalendar ->
            (if wide then
                viewPage model

             else
                viewPageMobile model
            )
                |> frame

        ViewSettings ->
            model.auth
                |> unwrap
                    ([ [ text "You're a guest, you don't have settings!" ]
                        |> paragraph [ varela, Font.center ]
                     , btn "Sign up now" (NavigateTo RouteHome)
                        |> el [ centerX ]
                     , hairline
                     , btn "Load example data" FakeData
                        |> el [ centerX ]
                     ]
                        |> column [ spacing 20, padding 20, centerX ]
                    )
                    (\_ ->
                        [ btn2 False Icons.save "Export posts" ExportPosts
                        , btn2 model.inProgress.logout Icons.power_off "Logout" Logout
                            |> el [ centerX ]
                        ]
                            |> column [ spacing 20 ]
                    )
                |> el
                    [ centerX
                    ]
                |> frame

        ViewStats ->
            "Coming soon"
                |> text
                |> el [ ebg, Font.size 30, centerX ]
                |> frame

        ViewTags ->
            (if wide then
                viewTags model

             else
                viewTagsMobile model
            )
                |> frame

        ViewSuccess ->
            [ text "BOLSTER"
                |> el
                    [ varela
                    , Font.semiBold
                    , Font.size 75
                    , style "animation-name" "fadeIn"
                    , style "animation-duration" "1s"
                    ]
            , text "Thank you for your purchase."
                |> el [ centerX ]
            , text "Please check your email for the next steps."
                |> el [ centerX ]
            , btn "Continue" (NavigateTo RouteHome)
                |> el [ centerX ]
            ]
                |> column [ centerX, spacing 20, padding 40 ]
    ]
        |> column
            [ spacing wd
            , height fill
            , width fill

            --, Element.clip
            , fShrink
            ]
        |> render model.isMobile


render : Bool -> Element msg -> Html msg
render isMobile =
    let
        disableUserSelect =
            [ "", "-ms-", "-moz-", "-webkit-" ]
                |> List.map
                    (\prefix ->
                        style (prefix ++ "user-select") "none"
                    )

        removeTapColor =
            style "-webkit-tap-highlight-color" "transparent"
    in
    Element.layoutWith
        { options =
            [ Element.focusStyle
                { borderColor =
                    if isMobile then
                        Nothing

                    else
                        Just <| Element.rgb255 255 0 0
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


viewTagsMobile : Model -> Element Msg
viewTagsMobile model =
    let
        tags =
            model.tags
                |> UD.values
    in
    model.tag
        |> Maybe.andThen
            (\t ->
                UD.get t model.tags
            )
        |> unwrap
            (if List.isEmpty tags then
                [ Input.text
                    [ Border.rounded 0
                    , Border.width 1
                    , width fill
                    , padding 10
                    , onKeydown [ onEnter TagCreateSubmit ]
                    ]
                    { onChange = TagCreateNameUpdate
                    , label = Input.labelHidden ""
                    , placeholder =
                        text "Create your first tag"
                            |> Input.placeholder []
                            |> Just
                    , text = model.tagCreateName
                    }
                , btn3 model.inProgress.tag Icons.send "Submit" TagCreateSubmit
                    |> el [ Element.alignRight, paddingXY 5 0 ]
                ]
                    |> column [ width fill, spacing 10, centerY ]

             else
                [ tags
                    |> List.map
                        (\t ->
                            Input.button
                                [ Font.size 25
                                , width fill
                                , padding 5
                                ]
                                { onPress = Just <| TagSelect <| Just t.id
                                , label =
                                    [ text t.name
                                    , text <| String.fromInt t.count
                                    ]
                                        |> row
                                            [ spaceEvenly
                                            , Background.color grey
                                            , padding 15
                                            , Border.rounded 15
                                            , width fill
                                            , Border.shadow
                                                { offset = ( 3, 3 )
                                                , blur = 3
                                                , size = 1
                                                , color = Element.rgb255 150 150 150
                                                }
                                            ]
                                }
                        )
                    |> column
                        [ spacing 10
                        , scrollbarY
                        , style "min-height" "auto"
                        , width fill
                        , height fill
                        ]
                , if model.tagCreate then
                    [ Input.text
                        [ Border.rounded 0
                        , Border.width 1
                        , width fill
                        , padding 10
                        , onKeydown [ onEnter TagCreateSubmit ]
                        ]
                        { onChange = TagCreateNameUpdate
                        , label = Input.labelHidden ""
                        , placeholder =
                            text "New tag"
                                |> Input.placeholder []
                                |> Just
                        , text = model.tagCreateName
                        }
                    , [ lnk "Cancel" TagCreateToggle
                      , btn2 model.inProgress.tag Icons.send "Submit" TagCreateSubmit
                      ]
                        |> row [ spacing 10, Element.alignRight ]
                    ]
                        |> column [ spacing 10, width fill ]

                  else
                    { onPress = Just TagCreateToggle
                    , label =
                        icon Icons.add 30
                            |> el [ centerX, centerY ]
                    }
                        |> rBtn
                        |> el [ Element.alignBottom, Element.alignRight ]
                ]
                    |> column
                        [ spacing 10
                        , width fill
                        , height fill
                        ]
            )
            (viewTag model)


viewTag : Model -> Tag -> Element Msg
viewTag model t =
    let
        ts =
            model.posts
                |> Day.values
                |> List.filter
                    (\p ->
                        List.member t.id p.tags
                    )
    in
    [ if model.tagBeingEdited == Just t.id then
        [ Input.text
            [ Border.rounded 0
            , Border.color black
            , width fill
            , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
            , onKeydown [ onEnter <| TagUpdateSubmit t ]
            ]
            { label = Input.labelHidden ""
            , onChange = TagUpdate
            , placeholder = Nothing
            , text = model.tagUpdate
            }
        , { onPress = Just <| TagUpdateSet Nothing
          , label =
                icon Icons.close 30
                    |> el [ centerX, centerY ]
          }
            |> rBtn
        , { onPress = Just <| TagUpdateSubmit t
          , label =
                icon Icons.send 30
                    |> el [ centerX, centerY ]
          }
            |> rBtn
        ]
            |> row [ width fill, spacing 10 ]

      else
        Input.button [ width fill, padding 10 ]
            { onPress = Just <| TagUpdateSet <| Just t
            , label =
                [ text t.name
                , icon Icons.edit 20
                ]
                    |> row [ spaceEvenly, width fill ]
            }
    , if List.isEmpty ts then
        [ text "No posts with this tag."
            |> el [ centerX ]
        , btn
            "Go to calendar"
            (NavigateTo RouteCalendar)
            |> el [ centerX ]
        ]
            |> column [ spacing 20, padding 20, centerX ]

      else
        ts
            |> List.map
                (\p ->
                    Input.button [ Font.size 25 ]
                        { onPress =
                            RouteDay p.date
                                |> NavigateTo
                                |> Just
                        , label =
                            [ p.date |> formatDay |> text
                            , [ p.body
                                    |> Maybe.withDefault ""
                                    |> text
                              ]
                                |> paragraph []
                            ]
                                |> column
                                    [ spacing 10
                                    , Border.width 1
                                    , padding 10
                                    , width fill
                                    ]
                        }
                )
            |> column
                [ spacing 10
                , width fill
                , Element.scrollbarY
                , height fill
                ]
    , [ btn2 False Icons.delete "Delete" <| TagDelete t
      , { onPress = Just <| TagSelect Nothing
        , label =
            icon Icons.undo 30
                |> el [ centerX, centerY ]
        }
            |> rBtn
      ]
        |> row [ spacing 20, Element.alignBottom, Element.alignRight ]
    ]
        |> column [ height fill, width fill, spacing 20 ]


viewTags : Model -> Element Msg
viewTags model =
    [ [ [ Input.text
            [ Border.rounded 0
            , Border.width 1
            , width fill
            , padding 10
            , onKeydown [ onEnter TagCreateSubmit ]
            ]
            { onChange = TagCreateNameUpdate
            , label = Input.labelHidden ""
            , placeholder =
                text "New tag"
                    |> Input.placeholder []
                    |> Just
            , text = model.tagCreateName
            }
        , btn "Submit" TagCreateSubmit
        ]
            |> row [ spacing 20 ]
      , model.tags
            |> UD.values
            |> List.map
                (\t ->
                    if model.tagBeingEdited == Just t.id then
                        Input.text
                            [ Border.rounded 0, shadow ]
                            { label =
                                Input.labelRight [] <|
                                    row []
                                        --[ emoji Tick <| TagUpdateSubmit { t | name = model.tagUpdate }
                                        --, emoji X <| TagUpdateSet Nothing
                                        --]
                                        []
                            , onChange = TagUpdate
                            , placeholder = Nothing
                            , text = model.tagUpdate
                            }

                    else
                        [ Input.button []
                            { onPress = Just <| TagSelect <| Just t.id
                            , label = text t.name
                            }

                        --(t.name ++ " [" ++  ++ "]")
                        --<|
                        --NavigateTo <|
                        --RouteTagPosts t.id
                        , text <| String.fromInt t.count
                        , row []
                            --[ emoji Edit <| TagUpdateSet <| Just t
                            --, emoji Trash <| TagDelete t
                            --]
                            []
                        ]
                            |> row [ spacing 20, width fill ]
                )
            |> column []
      ]
        |> column [ cappedWidth 450, centerX, Element.alignTop ]
    , model.tag
        |> whenJust
            (\t ->
                model.posts
                    |> Day.values
                    |> List.filter
                        (\p ->
                            List.member t p.tags
                        )
                    |> List.map
                        (\p ->
                            [ p.date |> formatDay |> text
                            , [ p.body
                                    |> Maybe.withDefault ""
                                    |> text
                              ]
                                |> paragraph []
                            ]
                                |> column [ spacing 10, Border.width 1, padding 10 ]
                        )
                    |> column
                        [ spacing 10
                        , cappedWidth 600
                        , Element.scrollbarY
                        , Element.alignTop

                        --, height fill
                        , height <| px 700
                        ]
            )
    ]
        |> row [ centerX, spacing 50, height fill ]


viewHomeMobile : Model -> Element Msg
viewHomeMobile model =
    let
        xs =
            model.screen.width < 360
    in
    [ [ [ text "BOLSTER"
            |> el
                [ Font.size 65
                , Font.semiBold
                , abel
                ]
        , View.Img.tmp 85
            |> Element.html
            |> el []
        ]
            |> row
                [ spacing 20

                --, style "animation-name" "fadeIn"
                --, style "animation-duration" "1s"
                , centerX
                , padding 10
                ]
      , viewInfo model.def
      ]
        |> column
            [ width fill
            , padding 20
            ]
    , [ viewFunnel model
      , Input.button
            [ centerX
            , centerY
            , Font.size 25
            , varela
            , Border.rounded 50
            , padding 20

            --, shadow
            , Border.shadow
                { offset = ( 4, 4 )
                , blur = 4
                , size = 0
                , color = Element.rgb255 150 150 150
                }

            --, Background.gradient
            --{ angle = 0
            --, steps =
            --[ Element.rgb255 150 208 255
            --, Element.rgb255 13 50 77
            --]
            --}
            , Font.color black
            , Background.color grey
            ]
            { onPress = Just <| NavigateTo RouteCalendar
            , label = text "Try demo"
            }
            |> el [ padding 20, Element.alignRight, Element.alignBottom ]
      ]
        |> column
            [ width fill
            ]
    ]
        |> column
            [ height fill
            , viewFaq model
                |> Element.inFront
            , width fill
            , spaceEvenly
            , fShrink

            --, Element.clip
            ]


viewBuy model =
    let
        waiting =
            not model.inProgress.monthlyPlan && not model.inProgress.annualPlan

        monthlyCursor =
            if model.inProgress.monthlyPlan then
                "wait"

            else if model.inProgress.annualPlan then
                "not-allowed"

            else
                "pointer"

        annualCursor =
            if model.inProgress.annualPlan then
                "wait"

            else if model.inProgress.monthlyPlan then
                "not-allowed"

            else
                "pointer"
    in
    [ text "Sign up"
        |> el [ Font.italic, Font.size 25, Font.bold, centerX ]
    , [ text "Monthly plan"
      , Input.button
            [ Background.gradient
                { angle = degrees 0
                , steps =
                    [ Element.rgb255 225 95 137
                    , Element.rgb255 13 50 77
                    ]
                }
            , width <| px 150
            , height <| px 40
            , Font.center
            , Font.color white
            , Border.rounded 10
            , Border.shadow
                { offset = ( 4, 4 )
                , blur = 4
                , size = 0
                , color = Element.rgb255 150 150 150
                }
            , Element.mouseDown
                [ Element.moveRight 5
                , Element.moveDown 5
                , Border.shadow
                    { offset = ( 0, 0 )
                    , blur = 0
                    , size = 0
                    , color = Element.rgb255 150 150 150
                    }
                ]
                |> whenAttr waiting
            , style "cursor" monthlyCursor
            ]
            { onPress =
                if waiting then
                    Just <| Buy False

                else
                    Nothing
            , label =
                if model.inProgress.monthlyPlan then
                    icon Icons.refresh 25
                        |> el [ rotate, centerX ]

                else
                    text "$5"
            }
      ]
        |> row [ spaceEvenly, width fill ]
    , [ text "Annual plan"
      , Input.button
            [ Background.gradient
                { angle = degrees 0
                , steps =
                    [ Element.rgb255 225 95 137
                    , Element.rgb255 13 50 77
                    ]
                }
            , width <| px 150
            , height <| px 40
            , Font.center
            , Font.color white
            , Border.rounded 10
            , Border.shadow
                { offset = ( 4, 4 )
                , blur = 4
                , size = 0
                , color = Element.rgb255 150 150 150
                }
            , Element.mouseDown
                [ Element.moveRight 5
                , Element.moveDown 5
                , Border.shadow
                    { offset = ( 0, 0 )
                    , blur = 0
                    , size = 0
                    , color = Element.rgb255 150 150 150
                    }
                ]
                |> whenAttr waiting
            , style "cursor" annualCursor
            ]
            { onPress =
                if waiting then
                    Just <| Buy True

                else
                    Nothing
            , label =
                if model.inProgress.annualPlan then
                    icon Icons.refresh 25
                        |> el [ rotate, centerX ]

                else
                    text "$40"
            }
      ]
        |> row [ spaceEvenly, width fill ]
    , [ Element.image
            [ height <| px 35

            --, fadeIn
            , style "animation" "enter 1s"
            , style "transform-origin" "center"
            ]
            { src = "/stripe1.png", description = "" }
      , lnk "Back" Change
      ]
        |> row [ spaceEvenly, width fill ]
    ]
        |> column
            [ centerX
            , spacing 20
            , width fill
            , padding 20
            , Element.alignBottom
            , Background.color grey

            --, style "animation" "enter 1s"
            , style "transform-origin" "center"
            , shadow2
            , Border.rounded 20
            ]


viewInfo mDef =
    [ [ text "The"
            |> el
                [ Element.paddingEach
                    { top = 0
                    , bottom = 9
                    , left = 0
                    , right = 0
                    }
                ]

      --|> viewDef model.def Types.Alts
      , [ "secure"
            |> viewDef mDef Types.Secure
        , text ","
        ]
            |> row []
      , "private"
            |> viewDef mDef Types.Private
      , [ "journal"
            |> viewDef mDef Types.Journal
        , text "."
        ]
            |> row []
      ]
        |> row
            [ spacing 5
            , Font.italic
            , Font.size 20
            , varela
            , centerX
            ]
    , mDef
        |> whenJust
            (\d ->
                (case d of
                    Alts ->
                        [ text "Information about alternative products can be found here." ]

                    Secure ->
                        [ text "Built for performance and security, using the leading technologies available."
                        ]

                    Private ->
                        [ text "Everything you write is protected by your password before it is saved, ensuring only you can ever read it." ]

                    Journal ->
                        [ text "For everyday use, on every device." ]
                )
                    |> paragraph
                        [ padding 20

                        --, width Element.shrink
                        , Background.color grey
                        , ebg
                        , Font.size 20
                        ]
            )
    ]
        |> column
            [ width fill
            ]


viewFaq : Model -> Element Msg
viewFaq model =
    [ Input.button
        [ centerX
        , Font.size 25
        , varela
        , padding 20
        , Font.color black
        ]
        { onPress = Just FaqToggle
        , label = text "FAQ"
        }
    , [ [ "Copper mug artisan messenger bag vaporware tumeric try-hard mlkshk quinoa waistcoat salvia taxidermy williamsburg."
        , "Lomo raclette gentrify shoreditch polaroid venmo."
        , "Kitsch cornhole bicycle rights, YOLO jean shorts prism direct trade sustainable marfa."
        , "Taiyaki biodiesel you probably haven't heard of them poke post-ironic chia skateboard squid craft beer tote bag."
        , "Portland palo santo tacos neutra deep v, kale chips selvage skateboard synth."
        , "Ramps pour-over hexagon edison bulb, hammock prism chartreuse gastropub iceland freegan."
        ]
            |> List.map
                (\b ->
                    [ [ text "Blah blah blah?" ]
                        |> paragraph [ Font.bold ]
                    , [ text b ]
                        |> paragraph [ Font.italic ]
                    ]
                        |> column [ spacing 5, height fill, width fill ]
                )
            |> column
                [ spacing 10
                , height fill
                , Element.scrollbarY
                , style "min-height" "auto"
                , width fill
                ]
      , [ Element.newTabLink [ Font.size 15, Font.italic ]
            { url = "https://tarbh.engineering"
            , label = paragraph [] [ text "Made by Tarbh" ]
            }
        , Input.button
            [ Font.size 25
            , varela
            , Font.color black
            ]
            { onPress = Just FaqToggle
            , label =
                [ icon Icons.keyboard_return 25, text "Back" ]
                    |> row [ spacing 10, height <| px 50 ]
            }
        ]
            |> row [ width fill, spaceEvenly ]
      ]
        |> column
            [ spacing 10
            , padding 10
            , height fill
            , width fill

            --, Element.clip
            , fShrink
            ]
        |> when model.faq
    ]
        |> column
            [ Border.shadow
                { offset = ( 4, 4 )
                , blur = 4
                , size = 0
                , color = Element.rgb255 150 150 150
                }
            , Background.color grey
            , Border.rounded 25
            , height fill
            , width fill
            ]
        |> List.singleton
        |> row
            ([ Element.paddingEach
                { top = 10
                , bottom = 20
                , left = 20
                , right = 20
                }

             --, Element.clip
             , fShrink
             , Element.alignBottom
             ]
                ++ (if model.faq then
                        [ height <| px 450
                        , width fill
                        ]

                    else
                        [ Element.alignLeft
                        ]
                   )
            )


viewHome : Model -> Element Msg
viewHome model =
    [ [ text "BOLSTER"
            |> el
                [ Font.size 150
                , Font.semiBold
                , abel
                ]
      , View.Img.tmp 150
            |> Element.html
            |> el []
      ]
        |> row
            [ spacing 40
            , style "animation-name" "fadeIn"
            , style "animation-duration" "1s"
            , centerX
            , padding 40
            ]
    , [ [ Element.image
            [ width <| px 200
            , style "animation-name" "fadeIn"
            , style "animation-duration" "1s"
            ]
            { src = "/phone.png"
            , description = ""
            }
        , Input.button
            [ Font.underline
            , Element.mouseOver
                [ Font.color blue
                ]
            , centerX
            , Font.size 25
            , varela
            ]
            { onPress = Just <| NavigateTo RouteCalendar
            , label = text "Try demo"
            }
        ]
            |> column [ spacing 10 ]
      , [ viewInfo model.def
            |> el
                [ width <| px 420
                , Element.alignTop
                ]
        , viewFunnel model
        ]
            |> column [ spaceEvenly, height fill ]
      ]
        |> row
            [ spacing 40
            , centerX
            ]
    ]
        |> column
            [ spacing 30
            , width fill
            ]


viewDef : Maybe Def -> Def -> String -> Element Msg
viewDef curr def str =
    Input.button
        [ Font.underline
            |> whenAttr (Just def /= curr)
        , style "text-decoration-style" "dashed"
        , Element.mouseOver
            [ Font.color blue
            ]
        , Background.color grey
            |> whenAttr (Just def == curr)

        --, Font.color blue
        , Element.paddingEach { top = 5, bottom = 15, left = 5, right = 5 }
        ]
        { onPress = Just <| SetDef def
        , label = text str
        }


viewEmail : Model -> Element Msg
viewEmail model =
    let
        valid =
            isValidEmail model.loginForm.email
    in
    [ [ Input.email
            [ Border.rounded 0
            , Border.width 0
            , height <| px 50
            , width <| px 275
            , style "cursor" "wait"
                |> whenAttr model.inProgress.login
            , Html.Attributes.disabled model.inProgress.login
                |> Element.htmlAttribute
            , (if model.funnel /= Types.Hello then
                grey

               else
                white
              )
                |> Background.color
            , onKeydown [ onEnter EmailSubmit ]
                |> whenAttr valid
            , Border.widthEach { bottom = 1, top = 1, left = 1, right = 0 }
            , Border.color black
            ]
            { onChange = LoginFormEmailUpdate
            , label = Input.labelHidden ""
            , placeholder =
                text "Your email address"
                    |> Input.placeholder [ varela ]
                    |> Just
            , text = model.loginForm.email
            }
      , btn2 model.inProgress.login Icons.send "Submit" EmailSubmit
      ]
        |> row
            [ centerX
            ]
    ]
        |> column
            [ spacing 20
            ]


viewFunnel : Model -> Element Msg
viewFunnel model =
    let
        ent =
            if model.funnel == Hello then
                EmailSubmit

            else
                Change

        valid =
            isValidEmail model.loginForm.email
    in
    case model.funnel of
        Hello ->
            [ Input.email
                [ Border.rounded 0
                , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }

                --, Border.width 0
                --, height <| px 50
                , paddingXY 0 10

                --, style "cursor" "wait"
                --|> whenAttr model.inProgress
                , width fill
                , onKeydown [ onEnter ent ]

                --|> whenAttr valid
                ]
                { onChange = LoginFormEmailUpdate
                , label = Input.labelHidden ""
                , placeholder =
                    text "Your email address"
                        |> Input.placeholder []
                        |> Just
                , text = model.loginForm.email
                }

            --, el [ height fill, width <| px 1, Background.color black ] none
            , Input.button
                [ --, Background.color darkBlue
                  --, Font.color white
                  --, style "cursor" "wait"
                  --|> whenAttr model.inProgress
                  width <| px 120
                , height fill
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
                    if valid then
                        Just ent

                    else
                        Nothing
                , label =
                    (if model.funnel /= Types.Hello then
                        "Change"

                     else
                        "Continue"
                    )
                        |> text
                        |> el [ centerX ]
                }
                |> when False
            , btn2 model.inProgress.login Icons.send "Submit" ent
                |> el [ Element.alignRight ]
            ]
                |> column
                    [ spacing 10

                    --, Element.alignRight
                    --, style "cursor" "wait"
                    --|> whenAttr model.inProgress
                    --, (if model.inProgress then
                    --grey
                    --else
                    --white
                    --)
                    --|> Background.color
                    --, padding 10
                    --, Border.width 1
                    , paddingXY 20 0
                    , width fill
                    ]

        WelcomeBack nonce ->
            viewWelcome model nonce

        CheckEmail ->
            [ text "Please check your email for signup instructions."
            , btn "Re-send email" (NavigateTo RouteHome)
                |> el [ centerX ]
            ]
                |> column
                    [ centerX
                    , spacing 20
                    ]

        JoinUs ->
            viewBuy model
                |> el [ paddingXY 20 0, width fill ]


vb2 model =
    [ text "Please choose your desired plan"
        |> el [ Font.italic ]
    , [ text "$5 per month"
      , btn "Buy" (Buy False)
      ]
        |> row [ spacing 20, centerX ]
    , [ text "$40 per year"
      , btn "Buy" (Buy True)
      ]
        |> row [ spacing 20, centerX ]
    , lnk "Back" Change
        |> el [ Element.alignRight ]
    ]
        |> column
            [ spacing 20
            , shadow2
            , padding 20
            , Background.color grey
            , Border.rounded 20
            , centerX
            ]


viewWelcome : Model -> String -> Element Msg
viewWelcome model nonce =
    [ text "Welcome back"
        |> el [ Font.italic ]
    , [ Input.currentPassword
            [ Border.rounded 0
            , Border.width 0
            , width fill
            , padding 10
            , centerY
            , onKeydown [ onEnter <| LoginSubmit nonce ]
            ]
            { onChange = LoginFormPasswordUpdate
            , label = Input.labelHidden ""
            , show = False
            , placeholder =
                text "Your password"
                    |> el [ centerY ]
                    |> Input.placeholder []
                    |> Just
            , text = model.loginForm.password
            }
            |> el
                [ height fill
                , width fill
                , Border.widthEach { top = 1, bottom = 1, left = 1, right = 0 }
                , Border.color black
                ]
      , btn "Submit" (LoginSubmit nonce)
      ]
        |> row
            [ width fill
            ]
    ]
        |> column
            [ width fill
            , spacing 20
            ]


viewFrame : Model -> Element Msg -> Element Msg
viewFrame model elem =
    let
        wd =
            if model.screen.height <= 768 then
                10

            else
                20
    in
    [ [ Input.button
            [ Font.semiBold
            ]
            { onPress = Just <| NavigateTo RouteHome
            , label =
                [ text "BOLSTER" |> el [ abel, Font.size 30 ]
                , text "DEMO"
                    |> el [ Font.light, varela ]
                    |> when (model.auth == Nothing)
                ]
                    |> row [ spacing 10 ]
            }
      , [ ( "Calendar", RouteCalendar, ViewCalendar )
        , ( "Tags", RouteTags, ViewTags )

        --, ( "Stats", RouteStats, ViewStats )
        , ( "Settings", RouteSettings, ViewSettings )
        ]
            |> List.map
                (\( n, r, v ) ->
                    Input.button
                        [ Font.underline |> whenAttr (v == model.view)
                        , Element.mouseOver
                            [ Font.color blue
                            ]
                        ]
                        { onPress =
                            if v == model.view then
                                Nothing

                            else
                                Just <| NavigateTo r
                        , label = text n
                        }
                )
            |> row [ spacing 40 ]
      ]
        |> row [ width fill, spaceEvenly, paddingXY 20 wd ]
    , elem
    ]
        |> column [ spacing wd, height fill, cappedWidth 1275, centerX ]


viewFrameMobile : Model -> Element Msg -> Element Msg
viewFrameMobile model elem =
    let
        nd =
            if model.tall then
                20

            else
                10

        pic =
            if model.tall then
                50

            else
                35
    in
    [ [ Input.button []
            { onPress = Just <| NavigateTo RouteHome
            , label =
                [ View.Img.tmp pic
                    |> Element.html
                    |> el []
                , text "DEMO"
                    |> el
                        [ Font.semiBold
                        , Font.size 25
                        ]
                    |> when (model.auth == Nothing)
                ]
                    |> row [ spacing 10 ]
            }
      , (case model.view of
            ViewCalendar ->
                "Calendar"

            ViewTags ->
                "Tags"

            _ ->
                "Settings"
        )
            |> text
            |> el [ Font.italic ]
            |> when model.tall
      ]
        |> row
            [ width fill
            , spaceEvenly

            --, paddingXY 20 10
            , [ Input.button [ Element.alignRight ]
                    { onPress = Just DropdownToggle
                    , label = icon Icons.menu 40
                    }
              , [ viewRoute Icons.calendar_today "Calendar" RouteCalendar ViewCalendar model.view
                , viewRoute Icons.label "Tags" RouteTags ViewTags model.view

                --, ( "Stats", RouteStats, ViewStats )
                , viewRoute Icons.settings "Settings" RouteSettings ViewSettings model.view
                ]
                    |> column [ spacing 10, padding 10 ]
                    |> when model.dropdown
              ]
                |> column
                    [ Background.color grey
                        |> whenAttr model.dropdown
                    , shadow2
                        |> whenAttr model.dropdown

                    --, padding 10
                    --|> whenAttr (not model.dropdown)
                    --, Border.width 1
                    --|> whenAttr model.dropdown
                    ]
                |> el
                    [ Element.alignRight

                    --, padding 10
                    --|> whenAttr model.dropdown
                    , style "z-index" "1"
                    ]
                |> when (not model.tall)
                |> Element.inFront
            ]
    , elem
    , [ ( Icons.settings, RouteSettings, ViewSettings )
      , ( Icons.calendar_today, RouteCalendar, ViewCalendar )
      , ( Icons.label, RouteTags, ViewTags )
      ]
        |> List.map
            (\( n, r, v ) ->
                Input.button
                    [ Font.underline |> whenAttr (v == model.view)
                    , width fill
                    , (if v == model.view then
                        blue

                       else
                        black
                      )
                        |> Font.color
                    ]
                    { onPress =
                        if v == model.view then
                            Nothing

                        else
                            Just <| NavigateTo r
                    , label =
                        icon n 40
                            |> el [ centerX, height fill ]
                    }
            )
        |> row [ Element.alignBottom, width fill ]
        |> when (model.tall && model.screen.height >= 660)
    ]
        |> column
            [ spacing nd
            , height fill
            , width fill

            --, Element.clip
            , fShrink
            , padding nd
            ]


viewRoute icn txt r v curr =
    Input.button
        [ Border.width 1 |> whenAttr (v == curr)
        , Element.mouseOver
            [ Font.color blue
            ]
        , Font.size 25
        , width fill
        , padding 10
        ]
        { onPress =
            (if v == curr then
                DropdownToggle

             else
                NavigateTo r
            )
                |> Just
        , label =
            [ icon icn 20
            , text txt
            ]
                |> row [ spacing 10 ]
        }


viewPage : Model -> Element Msg
viewPage model =
    let
        wd =
            if model.screen.height <= 768 then
                10

            else
                20
    in
    [ viewCalendar model
        |> el
            [ Element.alignTop
            ]
    , model.current
        |> unwrap
            viewReady
            (viewPost model)
    ]
        |> row [ width fill, height fill, spacing wd, paddingXY 20 wd ]


rBtn =
    Input.button
        [ Font.color black
        , Border.shadow
            { offset = ( 2, 2 )
            , blur = 3
            , size = 1
            , color = Element.rgb255 150 150 150
            }
        , Background.color grey
        , Border.rounded 25
        , padding 5
        ]


viewPageMobile : Model -> Element Msg
viewPageMobile model =
    let
        wd =
            fill

        flip =
            model.postBeingEdited || model.postView || model.tagView

        cal =
            viewCalendar model
                |> el
                    [ Element.alignTop
                    , width fill

                    --, style "transform-origin" "bottom center"
                    --, style "animation-fill-mode" "forwards"
                    --, style "animation" "fadeOut 0.5s"
                    --|> whenAttr flip
                    , Element.transparent flip
                    ]
    in
    [ cal
        |> when (not flip)
    , [ model.current
            |> whenJust (viewPost model)
            |> when flip
      , model.current
            |> whenJust (viewBarMobile model)
      ]
        |> column
            [ width fill
            , height fill

            --, Element.clip
            , fShrink
            , spacing 10
            ]
    ]
        |> column
            [ width fill
            , height fill

            --, paddingXY 20 10
            --, Element.clip
            , fShrink
            , cal
                |> when model.postBeingEdited
                |> el [ padding 20, width fill, height fill ]
                |> Element.behindContent
            ]


viewBarMobile : Model -> Date -> Element Msg
viewBarMobile model day =
    let
        pst =
            model.posts
                |> Day.get day
    in
    (if model.postBeingEdited then
        [ lnk "Cancel" PostUpdateCancel
        , btn2 model.inProgress.post
            Icons.save
            "Submit"
            (pst
                |> unwrap (PostCreateSubmit day)
                    (\p ->
                        if model.postEditorBody == "" then
                            PostClear p

                        else
                            PostUpdateSubmit p.id
                    )
            )
        ]

     else if model.tagView then
        [ btn2 False Icons.edit "Write" PostUpdateStart

        --, btn2 False Icons.close "Close" TagViewToggle
        , btn "X" TagViewToggle
        ]

     else if model.postView then
        [ btn2 False Icons.label "Tags" TagViewToggle
        , btn2 False Icons.edit "Edit" PostUpdateStart

        --, btn2 False Icons.close "Close" PostViewToggle
        , btn "X" PostViewToggle
        ]

     else
        [ btn2 False Icons.label "Tags" TagViewToggle
        , pst
            |> unwrap
                (btn2 False Icons.edit "Write" PostUpdateStart)
                (\p ->
                    p.body
                        |> unwrap
                            (btn2 False Icons.edit "Write" PostUpdateStart)
                            (\body ->
                                [ icon Icons.visibility 25
                                    |> el [ Element.alignTop ]
                                , [ text body ]
                                    |> paragraph
                                        [ Font.size 12

                                        --, style "overflow-y" "auto"
                                        , height <| px 50

                                        --, style "text-overflow" "ellipsis"
                                        , style "overflow" "hidden"
                                        ]
                                ]
                                    |> row
                                        [ spacing 10
                                        , Background.color grey
                                        , padding 10

                                        --, Border.shadow
                                        --{ offset = ( 2, 2 )
                                        --, blur = 0
                                        --, size = 1
                                        --, color = Element.rgb255 150 150 150
                                        --}
                                        , Border.width 1
                                        , Border.rounded 20
                                        ]
                            )
                 --btn2 False Icons.visibility "View" PostViewToggle
                )
        ]
    )
        |> row [ width fill, spaceEvenly, alignBottom, width fill ]


viewBar : Model -> Date -> Element Msg
viewBar model day =
    model.posts
        |> Day.get day
        |> unwrap
            (if model.postBeingEdited then
                [ btn2 model.inProgress.post Icons.save "Submit" (PostCreateSubmit day)
                , lnk "Cancel" PostUpdateCancel
                ]

             else
                [ btn2 False Icons.label "Tags" TagViewToggle
                , btn2 False Icons.edit "Write" PostUpdateStart
                ]
            )
            (\post ->
                if model.postBeingEdited || model.postView || model.tagView then
                    [ if model.postBeingEdited then
                        btn2
                            model.inProgress.post
                            Icons.save
                            "Submit"
                            (PostUpdateSubmit post.id)
                            |> when (Just model.postEditorBody /= post.body && model.postEditorBody /= "")

                      else
                        btn2
                            model.inProgress.post
                            Icons.edit
                            "Edit"
                            PostUpdateStart
                    , [ btn2 False Icons.delete "Delete" (PostDelete post.id post.date)
                            |> when False
                      , if model.postBeingEdited then
                            btn2 False Icons.close "Cancel" PostUpdateCancel

                        else
                            btn2 False Icons.close "Close" PostViewToggle
                      ]
                        |> row [ spacing 10, Element.alignRight ]
                    ]

                else
                    [ -- ellipsisText 15 post.body
                      [ btn2 False
                            Icons.edit
                            "Edit"
                            PostUpdateStart
                      , btn2 False
                            Icons.visibility
                            "View"
                            PostViewToggle
                      ]
                        |> row [ spacing 10, Element.alignRight ]
                    ]
            )
        |> row [ width fill, spaceEvenly, alignBottom, width fill ]


viewReady : Element Msg
viewReady =
    Input.button
        [ width fill
        , Helpers.View.cappedHeight 700
        , Background.color grey
        , style "cursor" Icon.pencil
        ]
        { onPress =
            RouteToday
                |> NavigateTo
                |> Just
        , label = none
        }


viewPost : Model -> Date -> Element Msg
viewPost model d =
    let
        fs =
            25

        pst =
            model.posts
                |> Day.get d

        xs =
            UD.values model.tags

        tMsg =
            pst
                |> unwrap (PostCreateWithTag d)
                    PostTagToggle

        data =
            model.posts
                |> Day.get d

        create =
            viewPostEditor (PostCreateSubmit d) model.postEditorBody False fs

        make post =
            viewPostEditor (PostUpdateSubmit post.id) model.postEditorBody (not model.postBeingEdited) fs

        txt =
            if model.tagView then
                Just "Tags"

            else
                pst
                    |> unwrap
                        (Just "Creating new entry")
                        (always
                            (if model.postBeingEdited then
                                Just "Updating entry"

                             else
                                Nothing
                            )
                        )

        topBar =
            if isWide model.screen then
                [ formatDay d
                    |> text
                    |> el [ width fill ]
                , [ lnk "Cancel" PostUpdateCancel
                        |> when (model.postBeingEdited && pst /= Nothing)
                  , if model.postBeingEdited then
                        btn2 model.inProgress.post
                            Icons.save
                            "Submit"
                            (pst
                                |> unwrap (PostCreateSubmit d)
                                    (\p ->
                                        if model.postEditorBody == "" then
                                            PostClear p

                                        else
                                            PostUpdateSubmit p.id
                                    )
                            )

                    else
                        btn2 False
                            Icons.edit
                            "Update"
                            PostUpdateStart
                  ]
                    |> row [ spacing 20 ]
                ]
                    |> row
                        [ spaceEvenly
                        , Font.size 17
                        , width fill
                        ]

            else
                [ formatDay d
                    |> text
                    |> el [ width fill ]
                , text "|"
                    |> when (txt /= Nothing)
                , txt
                    |> whenJust
                        (text
                            >> el [ Element.alignRight ]
                            >> el [ Font.italic, width fill ]
                        )
                ]
                    |> row [ spaceEvenly, Font.size 17, width fill ]
    in
    [ topBar
    , if model.tagView then
        viewPostTags model d pst

      else
        data
            |> unwrap create make
    ]
        |> column
            [ height fill
            , width fill
            , spacing 10

            --, style "animation" "rise 0.5s"
            --, style "transform-origin" "bottom"
            --, Element.clip
            , fShrink
            ]


viewPostEditor fn txt disable fontSize =
    Html.textarea
        [ Html.Attributes.id "editor"
        , Html.Attributes.value txt
        , Html.Attributes.style "font-size" "inherit"
        , Html.Attributes.style "font-family" "inherit"

        --, Html.Attributes.style "font-color" "black"
        , Html.Attributes.style "cursor" "inherit"
        , Html.Attributes.style "line-height" "30px"
        , Html.Attributes.style "padding" "0px"
        , Html.Attributes.style "flex-grow" "inherit"
        , Html.Attributes.readonly disable
        , Html.Events.onInput BodyUpdate
        ]
        []
        |> Element.html
        |> el
            [ width fill
            , (if disable then
                --Icon.pencil
                "default"

               else
                "text"
              )
                |> style "cursor"
            , height fill
            , Background.color grey
            , Font.size fontSize
            , padding 10
            , ebg

            --, Element.Events.onDoubleClick PostUpdateStart
            ]


viewPostTags : Model -> Date -> Maybe Post -> Element Msg
viewPostTags model d pst =
    let
        tMsg =
            pst
                |> unwrap (PostCreateWithTag d)
                    PostTagToggle

        xs =
            UD.values model.tags
    in
    if List.isEmpty xs then
        [ [ text "You don't have any tags." ]
            |> paragraph []
        , btn
            "Go to tags"
            (NavigateTo RouteTags)
            |> el [ centerX ]
        ]
            |> column [ spacing 20, padding 20, centerX ]

    else
        xs
            |> List.map
                (\t ->
                    let
                        flip =
                            pst |> unwrap False (\p -> List.member t.id p.tags)

                        prog =
                            List.member t.id model.inProgress.tags
                    in
                    --[ ellipsisText 20 t.name
                    [ paragraph [] [ text t.name ]
                    , Input.button
                        [ width <| px 40
                        , height <| px 40
                        , Border.width 1
                        , (if prog then
                            grey

                           else
                            white
                          )
                            |> Background.color
                        ]
                        { onPress =
                            if prog then
                                Nothing

                            else
                                Just <| tMsg t
                        , label =
                            if prog then
                                spinner

                            else
                                icon Icons.done 20
                                    |> el [ centerX, centerY ]
                                    |> when flip
                        }
                    ]
                        |> row
                            [ width fill
                            , spacing 10
                            ]
                )
            |> List.intersperse hairline
            |> column
                [ spacing 10
                , width fill
                , centerX
                , height fill
                , Element.scrollbarY
                , style "min-height" "auto"
                ]


shiftShadow : Attribute msg
shiftShadow =
    let
        color =
            rgb255 153 153 153
                |> colString
    in
    List.range 1 5
        |> List.map
            (String.fromInt
                >> (\n -> n ++ "px")
                >> (\n ->
                        "-" ++ n ++ " " ++ n ++ " 0px " ++ color
                   )
            )
        |> String.join ", "
        |> style "box-shadow"


{-| To handle scrollbarY problems.
<https://github.com/mdgriffith/elm-ui/issues/149>
-}
fShrink : Attribute msg
fShrink =
    style "flex-shrink" "1"
        |> whenAttr False


ellipsisText : Int -> String -> Element msg
ellipsisText n txt =
    Html.div
        [ Html.Attributes.style "overflow" "hidden"
        , Html.Attributes.style "text-overflow" "ellipsis"
        , Html.Attributes.style "white-space" "nowrap"
        , Html.Attributes.style "height" <| String.fromInt n ++ "px"
        , Html.Attributes.style "display" "table-cell"
        , Html.Attributes.title txt
        ]
        [ Html.text txt
        ]
        |> Element.html
        |> el
            [ width fill
            , style "width" "100%"
            , style "table-layout" "fixed"
            , style "display" "table"
            ]


hairline =
    el [ height <| px 1, width fill, Background.color black ] none


colString : Color -> String
colString =
    Element.toRgb
        >> (\{ red, green, blue } ->
                [ red * 255, green * 255, blue * 255 ]
           )
        >> List.map
            (round
                >> String.fromInt
            )
        >> String.join ", "
        >> (\str -> "rgb(" ++ str ++ ")")
