module View exposing (purple, textShadow, view, white, yellow)

import Calendar exposing (Day)
import Date exposing (Date)
import Day
import Element exposing (Attribute, Color, Element, centerX, centerY, column, el, fill, height, html, none, padding, paddingXY, paragraph, px, rgb255, row, spaceEvenly, spacing, text, width, wrappedRow)
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
import Icon
import Json.Decode as Decode exposing (Decoder)
import Material.Icons as Icons
import Material.Icons.Types exposing (Coloring(..), Icon)
import Maybe.Extra exposing (unwrap)
import Ordinal
import Time exposing (Month(..))
import Time.Format.I18n.I_en_us exposing (monthName)
import Types exposing (Def(..), Funnel(..), Model, Msg(..), Route(..), Sort(..), Status(..), View(..))
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
                Icons.chevron_right 50 Inherit
                    |> Element.html
                    |> el [ Element.moveUp 5, Font.color black ]
            }
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
                    |> row [ width fill ]
            )
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


grey : Color
grey =
    rgb255 255 245 235


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
    , case model.view of
        ViewHome ->
            viewHome model

        ViewMagic ->
            model.magic
                |> unwrap (text "wait")
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
            viewPage model

        ViewSettings ->
            model.auth
                |> unwrap
                    ([ text "You're a guest, you don't have settings!"
                     , btn "Sign up now" (NavigateTo RouteHome)
                        |> el [ centerX ]
                     ]
                        |> column [ spacing 50, centerX ]
                    )
                    (\_ ->
                        [ cool False Icons.save "Export posts" ExportPosts
                        , cool model.inProgress.logout Icons.power_off "Logout" Logout
                            |> el [ centerX ]
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
                                    , [ text p.body ]
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
                |> viewFrame model

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
        |> column [ spacing 20, height fill, width fill ]
        |> Element.layoutWith
            { options =
                [ Element.focusStyle
                    { borderColor = Just <| Element.rgb255 255 0 0
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
    [ [ ( "FAQ", RouteSettings )
      , ( "Contact", RouteSettings )
      , ( "Alternatives", RouteSettings )
      , ( "Security", RouteSettings )
      ]
        |> List.map
            (\( name, _ ) ->
                Input.button
                    [ Border.color white
                    , Border.widthEach { top = 0, bottom = 0, left = 10, right = 0 }
                    , Element.mouseOver [ Border.color black ]
                    ]
                    { onPress = Nothing
                    , label =
                        text name
                            |> el [ padding 10 ]
                    }
            )
        |> column
            [ Element.alignLeft
            , Element.alignTop
            , padding 50
            , Font.size 50
            ]
    , [ text "BOLSTER"
            |> el
                [ varela
                , Font.semiBold
                , Font.size 125
                , style "animation-name" "fadeIn"
                , style "animation-duration" "1s"
                ]
      , [ [ [ [ "The"
                    |> text
                    |> el
                        [ Element.alignTop
                        , Element.paddingEach { top = 5, bottom = 0, left = 0, right = 5 }
                        ]
              , "world's"
                    |> viewDef model.def Types.World
              , "first"
                    |> viewDef model.def Types.First
              , "private"
                    |> viewDef model.def Types.Private
              , "journal."
                    --|> viewDef model.def Types.Journal
                    |> text
                    |> el
                        [ Element.alignTop
                        , Element.paddingEach { top = 5, bottom = 0, left = 5, right = 0 }
                        ]
              ]
                |> row
                    [ spacing 5
                    , Font.italic
                    , Font.size 25
                    , Element.alignRight
                    ]
            , model.def
                |> whenJust
                    (\d ->
                        (case d of
                            World ->
                                [ text "Designed for every device." ]

                            First ->
                                [ text "There are no alternatives matching the features and cruising quality this product offers." ]

                            Private ->
                                [ text "All data is encypted with your password, ensuring only you can ever read it." ]

                            Journal ->
                                [ text "The sole focus of this application is your daily thoughts." ]
                        )
                            |> Element.paragraph
                                [ padding 20
                                , Background.color grey
                                , width <| px 450
                                , Element.alignRight
                                ]
                    )
            ]
                |> column [ width fill ]
          , Input.button
                [ Font.underline
                , Element.alignRight
                , Element.mouseOver
                    [ Font.color blue
                    ]
                ]
                { onPress = Just <| NavigateTo RouteCalendar
                , label = text "Try the demo"
                }
                |> when (model.auth == Nothing)
          ]
            |> column [ spacing 20, width fill ]
        , if model.auth == Nothing then
            viewFunnel model

          else
            btn "Return to app" (NavigateTo RouteCalendar)
                |> el [ Element.alignRight ]
        ]
            |> column [ spacing 40, Element.alignRight ]
      ]
        |> column
            [ spacing 20

            --, padding 75
            , Element.paddingXY 150 50
            , Element.alignTop
            , Element.alignRight
            ]
    ]
        |> row [ spacing 20, height fill, width fill ]


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
            , width <| px 350

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
            , [ text "$50 per year"
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
        |> column [ spacing 20 ]


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
    [ [ Input.button
            [ Font.family
                [ Font.typeface "Varela"
                ]
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
        |> row [ width fill, spaceEvenly, padding 20 ]
    , elem
    ]
        |> column [ spacing 20, height fill, cappedWidth 1450, centerX ]


viewPage : Model -> Element Msg
viewPage model =
    [ viewCalendar model
    , viewPost model
    ]
        |> row [ width fill, height fill, spacing 20, padding 20 ]
        |> viewFrame model


viewReady : Element Msg
viewReady =
    Input.button
        [ width fill
        , height <| px 700
        , Background.color grey
        , style "cursor" Icon.pencil
        ]
        { onPress =
            RouteToday
                |> NavigateTo
                |> Just
        , label = none
        }


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
                                , height <| px 700
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
                                , height <| px 700

                                --, Element.mouseOver [ Background.color grey ]
                                , Background.color grey
                                , padding 20
                                , Font.size 35
                                , Element.alignTop
                                , ebg
                                , style "cursor" Icon.pencil
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
                            ("Creating a new entry" |> text |> el [ Font.italic ])
                            (always
                                (("Updating entry" |> text |> el [ Font.italic ])
                                    |> when model.postBeingEdited
                                )
                            )
                    ]
                        |> row [ spacing 10 ]
                  , pst
                        |> unwrap
                            ([ cool model.postSaveInProgress Icons.save "Submit" (PostCreateSubmit d)
                                |> when (model.postEditorBody /= "")
                             , cool False Icons.close "Cancel" PostCancel
                             ]
                                |> row [ spacing 10 ]
                            )
                            (\p ->
                                if model.postBeingEdited then
                                    [ cool
                                        model.postSaveInProgress
                                        Icons.save
                                        "Submit"
                                        (PostUpdateSubmit p.id)
                                        |> when (model.postEditorBody /= p.body && model.postEditorBody /= "")
                                    , cool False Icons.delete "Delete" (PostDelete p.id p.date)
                                    , cool False Icons.close "Cancel" PostUpdateCancel
                                    ]
                                        |> row [ spacing 10 ]

                                else
                                    cool False
                                        (if model.postView then
                                            Icons.edit

                                         else
                                            Icons.label
                                        )
                                        (if model.postView then
                                            "Pad"

                                         else
                                            "Tags"
                                        )
                                        PostViewToggle
                            )
                  ]
                    |> row [ width fill, spaceEvenly, height <| px 55 ]
                , if model.postView then
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
                ]
                    |> column [ height fill, width fill, spacing 10 ]
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


formatDay : Date -> String
formatDay d =
    [ d |> Date.day |> Ordinal.ordinal
    , d
        |> Date.month
        |> monthToString
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


cool : Bool -> Icon msg -> String -> msg -> Element msg
cool inProg icon str msg =
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
        ]
        { onPress =
            if inProg then
                Nothing

            else
                Just msg
        , label =
            [ (if inProg then
                Icons.refresh

               else
                icon
              )
                |> (\ic ->
                        ic 25 Inherit
                   )
                |> Element.html
                |> el
                    [ rotate
                        |> whenAttr inProg
                    ]
            , text str
            ]
                |> row [ spacing 10 ]
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
        "rotation 2s infinite linear"
