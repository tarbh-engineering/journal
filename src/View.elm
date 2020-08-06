module View exposing (view)

import Calendar exposing (Day)
import Date exposing (Date)
import Day
import Element exposing (Attribute, Color, Element, alignBottom, centerX, centerY, column, el, fill, height, html, none, padding, paddingXY, paragraph, px, rgb255, row, scrollbarY, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
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
import Types exposing (Def(..), Funnel(..), Model, Msg(..), Route(..), Sort(..), Status(..), View(..))
import Validate exposing (isValidEmail)
import View.Misc exposing (btn, btn2, btn3, formatDay, icon, spinner)
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
        wd =
            if model.screen.width <= 1024 then
                55

            else
                75
    in
    [ [ Input.button [ Font.color white, Element.alignLeft ]
            { onPress = Just PrevMonth
            , label =
                icon Icons.chevron_left 50
                    |> el [ Element.moveUp 5, Font.color black ]
            }
      , [ model.month |> monthName |> text
        , model.year |> String.fromInt |> text
        ]
            |> row
                [ centerX
                , spacing 10
                , Background.color white
                , padding 10
                ]
      , Input.button [ Font.color white, Element.alignRight ]
            { onPress = Just NextMonth
            , label =
                icon Icons.chevron_right 50
                    |> el [ Element.moveUp 5, Font.color black ]
            }
      ]
        |> row [ width fill ]
    , Element.table
        [ spacing 10 ]
        { data = Calendar.weeks Time.Mon model.month model.year
        , columns =
            [ { header = weekday "Mon"
              , width = px wd
              , view = .mon >> viewCell model wd
              }
            , { header = weekday "Tue"
              , width = px wd
              , view = .tue >> viewCell model wd
              }
            , { header = weekday "Wed"
              , width = px wd
              , view = .wed >> viewCell model wd
              }
            , { header = weekday "Thu"
              , width = px wd
              , view = .thu >> viewCell model wd
              }
            , { header = weekday "Fri"
              , width = px wd
              , view = .fri >> viewCell model wd
              }
            , { header = weekday "Sat"
              , width = px wd
              , view = .sat >> viewCell model wd
              }
            , { header = weekday "Sun"
              , width = px wd
              , view = .sun >> viewCell model wd
              }
            ]
        }
    , model.current
        |> whenJust
            (\day ->
                [ ( '◀', Element.alignLeft, -1 )
                , ( '▶', Element.alignRight, 1 )
                ]
                    |> List.map
                        (\( char, attr, n ) ->
                            Input.button
                                [ Font.color blue
                                , Font.size 40
                                , Element.mouseOver [ Font.color black ]
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
                                , padding 20
                                , Border.width 2
                                ]
                                { onPress =
                                    Just <|
                                        NavigateTo <|
                                            RouteDay <|
                                                Day.shift n <|
                                                    day
                                , label =
                                    char
                                        |> String.fromChar
                                        |> text
                                }
                        )
                    |> row [ width fill, Element.alignBottom, paddingXY 0 50 ]
            )
    ]
        |> column [ spacing 10, height fill ]


cell2 : Model -> Day -> Element Msg
cell2 model day =
    let
        hv =
            model.posts
                |> Day.get day.date
                |> Maybe.andThen Helpers.extract
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
                |> Maybe.andThen Helpers.extract

        hv =
            pst
                |> unwrap False (always True)

        curr =
            Just day.date == model.current
    in
    Input.button
        [ height <| px n

        --, style "transition" "all 0.8s"
        , Html.Attributes.class "shift"
            |> Element.htmlAttribute
            |> whenAttr (not model.isMobile)

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
                |> text
                |> el [ Element.alignTop, Element.alignLeft, padding 5, Font.bold ]
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
        isMobile =
            model.screen.width <= 1024

        frame =
            if isMobile then
                viewFrameMobile model

            else
                viewFrame model

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
        |> when (not isMobile)
    , case model.view of
        ViewHome ->
            if isMobile then
                viewHomeMobile model

            else
                viewHome model

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
            (if isMobile then
                viewPageMobile model

             else
                viewPage model
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
            (if isMobile then
                viewTagsMobile model

             else
                viewTags model
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
            , Element.clip
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
    [ [ Input.text
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
      , btn3 model.inProgress.tag Icons.send "Submit" TagCreateSubmit
            |> el [ Element.alignRight, paddingXY 5 0 ]
      ]
        |> column [ width fill, spacing 10 ]
    , model.tags
        |> UD.values
        |> List.map
            (\t ->
                [ Input.button [ Font.size 25 ]
                    { onPress = Just <| TagSelect t.id
                    , label = text t.name
                    }
                , text <| String.fromInt t.count
                ]
                    |> row [ spaceEvenly, width fill ]
            )
        |> column
            [ spacing 10
            , scrollbarY
            , width fill
            , height fill
            , Background.color grey
            , padding 10
            ]
    ]
        |> column
            [ spacing 20
            , width fill
            , height fill
            , fShrink
            , Element.clip
            ]


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
                            { onPress = Just <| TagSelect t.id
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
                    |> List.filterMap Helpers.extract
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
    [ [ text "BOLSTER"
            |> el
                [ Font.size 55
                , Font.semiBold
                , abel
                ]
      , Element.image
            [ height <| px 60
            , width <| px 60
            ]
            { src = "/icons/192.png", description = "" }
      ]
        |> row
            [ spacing 20
            , style "animation-name" "fadeIn"
            , style "animation-duration" "1s"
            , centerX
            , padding 10
            ]
    , [ [ text "The"
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
                |> viewDef model.def Types.Secure
          , text ","
          ]
            |> row []
        , "private"
            |> viewDef model.def Types.Private
        , [ "journal"
                |> viewDef model.def Types.Journal
          , text "."
          ]
            |> row []
        ]
            |> row
                [ spacing 5
                , Font.italic
                , Font.size 20
                , centerX
                , varela
                ]
      , model.def
            |> whenJust
                (\d ->
                    (case d of
                        Alts ->
                            [ text "Information about alternative products can be found here." ]

                        Secure ->
                            [ text "Built for performance and security, using the leading technologies available. The code can be viewed"
                            , el [ width <| px 6 ] none
                            , Element.newTabLink [ Font.underline ]
                                { url = "https://github.com/tarbh-engineering/journal"
                                , label = text "here"
                                }
                            , text "."
                            ]

                        Private ->
                            [ text "Everything you write is protected by your password before it is saved, ensuring only you can ever read it." ]

                        Journal ->
                            [ text "For everyday use, on every device." ]
                    )
                        |> Element.paragraph
                            [ padding 20
                            , Background.color grey
                            , width fill
                            , Element.alignRight
                            , ebg
                            , Font.size 20
                            ]
                )
            |> when (model.funnel == Types.Hello)
      ]
        |> column
            [ width fill
            , paddingXY 20 0
            ]
    , [ case model.funnel of
            Hello ->
                none

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
                let
                    waiting =
                        not model.inProgress.monthlyPlan && not model.inProgress.annualPlan
                in
                [ text "Sign up"
                    |> el [ Font.italic, Font.size 25, Font.bold ]
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
                                text "$5 Buy"
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
                                text "$40 Buy"
                        }
                  ]
                    |> row [ spaceEvenly, width fill ]
                , Element.image [ height <| px 35, centerX, fadeIn ]
                    { src = "/stripe1.png", description = "" }
                ]
                    |> column
                        [ centerX
                        , spacing 20
                        , width fill
                        , padding 20
                        , Element.alignBottom
                        , Background.color grey
                        , style "animation" "enter 1s"
                        , style "transform-origin" "center"
                        ]
                    |> el [ width fill, paddingXY 20 0, Element.alignBottom ]
      , if model.thanks then
            "Thank you!"
                |> text
                |> el
                    [ style "animation-name" "fadeIn"
                    , style "animation-duration" "1s"
                    , centerX
                    , centerY
                    , Font.size 30
                    , varela
                    ]
                |> el [ width fill ]

        else
            viewFx model
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
            , height fill
            , viewFaq model
                |> Element.inFront
            ]
    ]
        |> column
            [ height fill
            , width fill
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
            , Element.clip
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
             , Element.clip
             , fShrink
             ]
                ++ (if model.faq then
                        [ height fill
                        , width fill
                        ]

                    else
                        [ Element.alignLeft
                        , Element.alignBottom
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
      , Element.image
            [ height <| px 150
            , width <| px 150
            ]
            { src = "/icons/192.png", description = "" }
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
            , centerX
            , style "animation-name" "fadeIn"
            , style "animation-duration" "1s"
            ]
            { src = "/phone.png"
            , description = ""
            }
        , Input.button
            [ Font.underline
            , Element.alignRight
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
            |> column [ spacing 10, width <| px 350 ]
      , [ [ [ [ text "The"
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
                    |> viewDef model.def Types.Secure
                , text ","
                ]
                    |> row []
              , "private"
                    |> viewDef model.def Types.Private
              , [ "journal"
                    |> viewDef model.def Types.Journal
                , text "."
                ]
                    |> row []
              ]
                |> row
                    [ spacing 5
                    , Font.italic
                    , Font.size 30
                    , Element.alignRight
                    , centerX
                    , varela
                    ]
            , model.def
                |> whenJust
                    (\d ->
                        (case d of
                            Alts ->
                                [ text "Information about alternative products can be found here." ]

                            Secure ->
                                [ text "Built for performance and security, using the leading technologies available. The code can be viewed"
                                , el [ width <| px 6 ] none
                                , Element.newTabLink [ Font.underline ]
                                    { url = "https://github.com/tarbh-engineering/journal"
                                    , label = text "here"
                                    }
                                , text "."
                                ]

                            Private ->
                                [ text "Everything you write is protected by your password before it is saved, ensuring only you can ever read it." ]

                            Journal ->
                                [ text "For everyday use, on every device." ]
                        )
                            |> Element.paragraph
                                [ padding 20
                                , Background.color grey
                                , width <| px 450
                                , Element.alignRight
                                , ebg
                                , Font.size 25
                                ]
                    )
            ]
                |> column []
          ]
            |> column [ spacing 20, centerX, height <| px 150 ]
        , if model.thanks then
            "Thank you!"
                |> text
                |> el
                    [ style "animation-name" "fadeIn"
                    , style "animation-duration" "1s"
                    , centerX
                    , centerY
                    , Font.size 30
                    , varela
                    ]
                |> el [ width fill ]

          else
            viewEmail model
        ]
            |> column [ spacing 50, height fill, width fill ]
      ]
        |> row
            [ spacing 40
            , height fill
            , centerX
            , width fill
            ]
    ]
        |> column
            [ spacing 30
            , centerX
            ]
        |> el
            [ height fill
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
    [ paragraph [ ebg, Font.size 25, width Element.shrink, centerX ]
        [ text "Coming soon. Sign up to be notified." ]
    , [ Input.email
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
            , width fill
            , Element.alignBottom
            , centerY
            ]


viewFx : Model -> Element Msg
viewFx model =
    let
        ent =
            if model.funnel == Hello then
                EmailSubmit

            else
                Change

        valid =
            isValidEmail model.loginForm.email
    in
    [ Input.email
        [ Border.rounded 0

        --, Border.width 1
        , cappedWidth 500
        , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }

        --, height <| px 50
        , paddingXY 0 15

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
        , onKeydown [ onEnter ent ]

        --|> whenAttr valid
        , Input.button
            [ Element.alignRight
            , centerY
            , Element.moveLeft 10
            , fadeIn
            ]
            { onPress = Just <| LoginFormEmailUpdate ""
            , label = text "X"
            }
            |> Element.inFront
            |> whenAttr (model.loginForm.email /= "" && not model.inProgress.login && model.funnel == Types.Hello)
        ]
        { onChange = LoginFormEmailUpdate
        , label = Input.labelHidden ""
        , placeholder =
            text "Your email address"
                |> Input.placeholder []
                |> Just
        , text = model.loginForm.email
        }
    , (if model.funnel == Types.Hello then
        btn2 model.inProgress.login Icons.send "Submit" ent

       else
        btn2 False Icons.keyboard_return "Back" Change
      )
        |> el [ Element.alignRight ]
    ]
        |> column [ spacing 10, alignBottom, paddingXY 20 0, width fill ]


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
    [ [ Input.email
            [ Border.rounded 0
            , Border.width 0
            , cappedWidth 350

            --, height <| px 50
            , paddingXY 20 15

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
      , el [ height fill, width <| px 1, Background.color black ] none
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
            none

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
            [ text "Please choose your desired plan"
                |> el [ Font.italic ]
            , [ text "$5 per month"
              , btn "Buy" (Buy False)
              ]
                |> row [ spaceEvenly, width fill ]
            , [ text "$40 per year"
              , btn "Buy" (Buy True)
              ]
                |> row [ spaceEvenly, width fill ]
            ]
                |> column
                    [ centerX
                    , spacing 20
                    , width <| px 200
                    ]
    ]
        |> column [ spacing 20, alignBottom, padding 30 ]


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
            [ varela
            , Font.semiBold
            , Font.size 25
            ]
            { onPress = Just <| NavigateTo RouteHome
            , label =
                [ text "BOLSTER"
                , text "DEMO"
                    |> el [ Font.light ]
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
        |> column [ spacing wd, height fill, cappedWidth 1450, centerX ]


viewFrameMobile : Model -> Element Msg -> Element Msg
viewFrameMobile model elem =
    [ [ Input.button
            [ varela
            , Font.semiBold
            , Font.size 25
            ]
            { onPress = Just <| NavigateTo RouteHome
            , label =
                [ Element.image
                    [ height <| px 35
                    , width <| px 35
                    ]
                    { src = "/icons/192.png", description = "" }
                , text "DEMO"
                    |> el [ abel ]
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
            |> when False
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
                    , Border.shadow
                        { offset = ( 3, 3 )
                        , blur = 4
                        , size = 2
                        , color = Element.rgb255 150 150 150
                        }
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
                |> Element.inFront
            ]
    , elem
    ]
        |> column
            [ spacing 10
            , height fill
            , width fill
            , Element.clip
            , fShrink
            , padding 10
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
    , viewPost model
    ]
        |> row [ width fill, height fill, spacing wd, paddingXY 20 wd ]


viewPageMobile : Model -> Element Msg
viewPageMobile model =
    let
        wd =
            fill

        ht =
            if model.screen.width <= 400 then
                40

            else
                50

        flip =
            model.postBeingEdited || model.postView || model.tagView

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

        cal =
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
            , model.current
                |> whenJust
                    (\day ->
                        [ ( Icons.arrow_back, Element.alignLeft, -1 )
                        , ( Icons.arrow_forward, Element.alignRight, 1 )
                        ]
                            |> List.map
                                (\( icn, attr, n ) ->
                                    Input.button
                                        [ Font.size 40
                                        , Element.mouseOver [ Font.color black ]
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
                                            day
                                                |> Day.shift n
                                                |> RouteDay
                                                |> NavigateTo
                                                |> Just
                                        , label = icon icn 30
                                        }
                                )
                            |> row [ width fill, Element.alignBottom ]
                    )
                |> when False
            ]
                |> column
                    [ spacing 10
                    , Element.alignTop
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
            |> whenJust (vp2 model)
            |> when flip
      , model.current
            |> whenJust (viewBarMobile model)
      ]
        |> column
            [ width fill
            , height fill
            , Element.clip
            , fShrink
            , spacing 10
            ]
    ]
        |> column
            [ width fill
            , height fill

            --, paddingXY 20 10
            , Element.clip
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
                |> Maybe.andThen Helpers.extract
    in
    (if model.postBeingEdited then
        [ btn2 False Icons.close "Cancel" PostUpdateCancel
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
        |> Maybe.andThen Helpers.extract
        |> unwrap
            (if model.postBeingEdited then
                [ btn2 model.inProgress.post Icons.save "Submit" (PostCreateSubmit day)
                , btn2 False Icons.close "Cancel" PostUpdateCancel
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


vp2 : Model -> Date -> Element Msg
vp2 model d =
    let
        fs =
            25

        pst =
            model.posts
                |> Day.get d
                |> Maybe.andThen Helpers.extract

        xs =
            UD.values model.tags

        fn fn1 =
            Html.textarea
                [ Html.Attributes.id "editor"
                , Html.Attributes.value model.postEditorBody
                , Html.Attributes.style "font-size" "inherit"
                , Html.Attributes.style "font-family" "inherit"
                , Html.Attributes.style "cursor" "inherit"
                , Html.Attributes.style "line-height" "30px"
                , Html.Attributes.style "padding" "0px"
                , Html.Attributes.style "flex-grow" "inherit"
                , Html.Events.onInput BodyUpdate
                ]
                []
                |> Element.html
                |> el
                    [ width fill
                    , style "cursor" "text"
                    , height fill
                    , Background.color grey
                    , Font.size fs
                    , padding 20
                    , ebg
                    ]

        tMsg =
            pst
                |> unwrap (PostCreateWithTag d)
                    PostTagToggle

        data =
            model.posts
                |> Helpers.getStatus d

        create =
            fn (PostCreateSubmit d)

        make post =
            if model.postBeingEdited then
                fn (PostUpdateSubmit post.id)

            else
                post.body
                    |> Maybe.withDefault ""
                    -- HACK: Need to pass through Html in order
                    -- to preserve formatting.
                    |> Html.text
                    |> List.singleton
                    |> Html.div
                        [ Html.Attributes.style "white-space" "pre-wrap"
                        , Html.Attributes.style "line-height" "30px"
                        , Html.Attributes.style "height" "100%"
                        , Html.Attributes.style "width" "100%"
                        , Html.Attributes.style "overflow-y" "auto"
                        , Html.Attributes.style "word-break" "break-word"
                        , Html.Attributes.style "min-height" "min-content"
                        ]
                    |> Element.html
                    |> el
                        [ width fill
                        , height fill
                        , Background.color grey
                        , padding 20
                        , Font.size fs
                        , ebg
                        , Element.clip
                        , fShrink
                        ]

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
    in
    [ [ formatDay d
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
    , if model.tagView then
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
                        [ ellipsisText 20 t.name
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
                    , cappedWidth 300
                    , centerX
                    , height fill
                    ]
                |> el
                    [ height fill
                    , width fill
                    , Element.scrollbarY
                    ]

      else
        case data of
            Missing ->
                create

            Loading ma ->
                ma
                    |> unwrap create make

            Found a ->
                make a
    ]
        |> column
            [ height fill
            , width fill
            , spacing 10
            , style "animation" "rise 0.5s"
            , style "transform-origin" "bottom"
            , Element.clip
            , fShrink
            ]


viewPostMobile : Model -> Element Msg
viewPostMobile model =
    model.current
        |> unwrap
            viewReady
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
                                , Element.alignTop
                                , height fill
                                , Background.color grey
                                , Font.size 35
                                , padding 20
                                , onKeydown [ onCtrlEnter fn1 ]
                                , ebg
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
                            post.body
                                |> Maybe.withDefault ""
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
                                    , Html.Attributes.style "min-height" "min-content"
                                    ]
                                |> Element.html
                                |> el
                                    [ width fill
                                    , height fill
                                    , Background.color grey
                                    , padding 20
                                    , Font.size 35
                                    , ebg
                                    , Element.clip
                                    , fShrink
                                    ]

                    pst =
                        model.posts
                            |> Day.get d
                            |> Maybe.andThen Helpers.extract
                in
                [ [ formatDay d
                        |> text
                  , text "|"
                        |> when model.postBeingEdited
                  , pst
                        |> unwrap
                            ("Creating new entry" |> text |> el [ Font.italic ])
                            (always
                                (("Updating entry" |> text |> el [ Font.italic ])
                                    |> when model.postBeingEdited
                                )
                            )
                  ]
                    |> row [ spaceEvenly, Font.size 17, width fill, height <| px 20 ]
                , if model.tagView then
                    let
                        xs =
                            UD.values model.tags
                    in
                    if List.isEmpty xs then
                        "Make a tag!"
                            |> text
                            |> el [ centerX, Font.bold ]

                    else
                        pst
                            |> unwrap
                                (xs
                                    |> List.map
                                        (\t ->
                                            Input.button
                                                [ padding 10
                                                , Border.width 1
                                                , (if List.member t.id model.postCreateTags then
                                                    blue

                                                   else
                                                    white
                                                  )
                                                    |> Background.color
                                                ]
                                                { onPress = Just <| PostCreateWithTag d t
                                                , label = text t.name
                                                }
                                        )
                                    |> Element.wrappedRow [ spacing 20 ]
                                )
                                (\p ->
                                    xs
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

                  else
                    case data of
                        Missing ->
                            create

                        Loading ma ->
                            ma
                                |> unwrap create make

                        Found a ->
                            make a
                ]
                    |> column
                        [ height fill
                        , width fill
                        , spacing 10
                        , style "animation" "rise 0.5s"
                        , style "transform-origin" "bottom"
                        , Element.clip
                        , fShrink
                        ]
            )


viewPost : Model -> Element Msg
viewPost model =
    model.current
        |> unwrap
            viewReady
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
                                , Element.alignTop
                                , Helpers.View.cappedHeight 700
                                , Background.color grey
                                , Font.size 35
                                , padding 20
                                , onKeydown [ onCtrlEnter fn1 ]
                                , ebg
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
                                , Helpers.View.cappedHeight 700

                                --, Element.mouseOver [ Background.color grey ]
                                , Background.color grey
                                , padding 20
                                , Font.size 35
                                , Element.alignTop
                                , ebg
                                , style "cursor" Icon.pencil
                                ]
                                { onPress = Just PostUpdateStart
                                , label =
                                    post.body
                                        |> Maybe.withDefault ""
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

                    pst =
                        model.posts
                            |> Day.get d
                            |> Maybe.andThen Helpers.extract
                in
                [ [ [ formatDay d
                        |> text
                    , text "|"
                        |> when (model.postBeingEdited || pst == Nothing)
                    , pst
                        |> unwrap
                            ("Creating new entry" |> text |> el [ Font.italic ])
                            (always
                                (("Updating entry" |> text |> el [ Font.italic ])
                                    |> when model.postBeingEdited
                                )
                            )
                    ]
                        |> row [ spacing 10 ]
                  ]
                    |> row [ width fill, spaceEvenly, height <| px 55 ]
                , if model.tagView then
                    pst
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

                  else
                    case data of
                        Missing ->
                            create

                        Loading ma ->
                            ma
                                |> unwrap create make

                        Found a ->
                            make a
                , pst
                    |> unwrap
                        ([ btn2 model.inProgress.post Icons.save "Submit" (PostCreateSubmit d)
                            |> when (model.postEditorBody /= "")
                         , btn2 False Icons.close "Cancel" PostCancel
                         ]
                            |> row [ spacing 10 ]
                        )
                        (\p ->
                            if model.postBeingEdited then
                                [ btn2
                                    model.inProgress.post
                                    Icons.save
                                    "Submit"
                                    (PostUpdateSubmit p.id)
                                    |> when (Just model.postEditorBody /= p.body && model.postEditorBody /= "")
                                , btn2 model.inProgress.postDelete Icons.delete "Delete" (PostDelete p.id p.date)
                                , btn2 False Icons.close "Cancel" PostUpdateCancel
                                ]
                                    |> row [ spacing 10 ]

                            else
                                btn2 False
                                    (if model.tagView then
                                        Icons.edit

                                     else
                                        Icons.label
                                    )
                                    (if model.tagView then
                                        "Pad"

                                     else
                                        "Tags"
                                    )
                                    PostViewToggle
                        )
                    |> el [ Element.alignRight, Element.alignBottom ]
                ]
                    |> column [ height fill, width fill, spacing 10 ]
            )


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
