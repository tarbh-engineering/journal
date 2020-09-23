module View exposing (view)

import Calendar exposing (Date)
import CalendarDates exposing (Day)
import CustomScalars exposing (Uuid)
import DateTime
import Day
import Element exposing (Attribute, Element, alignBottom, centerX, centerY, column, el, fill, height, html, none, padding, paddingXY, paragraph, px, rgb255, row, scrollbarY, spaceEvenly, spacing, text, width)
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
import Json.Decode as Decode exposing (Decoder)
import Material.Icons as Icons
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Time exposing (Month(..))
import Time.Format.I18n.I_en_us exposing (dayShort, monthName)
import Types exposing (Def(..), Funnel(..), Model, Msg(..), Post, Route, Tag, View(..))
import Validate exposing (isValidEmail)
import View.Img
import View.Misc exposing (btn, btn2, btn3, formatDateTime, formatDay, iBtn, icon, lnk, spinner)
import View.Style exposing (abel, black, blue, ebg, fadeIn, rotate, sand, varela, white)


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


shadow2 : Attribute msg
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
            if model.landscape then
                if model.screen.width < 800 then
                    40

                else
                    60

            else if model.area < 200000 then
                30

            else if model.area < 300000 then
                40

            else
                50

        wd =
            if model.screen.width < 450 then
                fill

            else
                px ht

        shift =
            case model.weekStart of
                Time.Mon ->
                    identity

                Time.Tue ->
                    cycle 1

                Time.Wed ->
                    cycle 2

                Time.Thu ->
                    cycle 3

                Time.Fri ->
                    cycle 4

                Time.Sat ->
                    cycle 5

                Time.Sun ->
                    cycle 6

        btnSize =
            if model.area < 200000 then
                20

            else
                30
    in
    [ [ iBtn btnSize Icons.chevron_left PrevMonth
      , [ model.month |> monthName |> text
        , model.year |> String.fromInt |> text
        ]
            |> row
                [ centerX
                , spacing 10
                , Background.color white
                , padding 10
                ]
      , iBtn btnSize Icons.chevron_right NextMonth
      ]
        |> row [ width fill, spaceEvenly, padding 5 ]
    , Element.table
        [ spacing 5 ]
        { data = CalendarDates.weeks model.weekStart model.month model.year
        , columns =
            [ { header = weekday <| dayShort Time.Mon
              , width = wd
              , view = .mon >> viewCell model ht
              }
            , { header = weekday <| dayShort Time.Tue
              , width = wd
              , view = .tue >> viewCell model ht
              }
            , { header = weekday <| dayShort Time.Wed
              , width = wd
              , view = .wed >> viewCell model ht
              }
            , { header = weekday <| dayShort Time.Thu
              , width = wd
              , view = .thu >> viewCell model ht
              }
            , { header = weekday <| dayShort Time.Fri
              , width = wd
              , view = .fri >> viewCell model ht
              }
            , { header = weekday <| dayShort Time.Sat
              , width = wd
              , view = .sat >> viewCell model ht
              }
            , { header = weekday <| dayShort Time.Sun
              , width = wd
              , view = .sun >> viewCell model ht
              }
            ]
                |> shift
        }
    ]
        |> column
            [ spacing 10
            , width fill
                |> whenAttr (model.screen.width < 450)
            , centerX
            ]


cycle : Int -> List a -> List a
cycle n xs =
    List.drop n xs ++ List.take n xs


viewCell : Model -> Int -> Day -> Element Msg
viewCell model n day =
    let
        pst =
            model.posts
                |> Day.get day.date

        curr =
            Just day.date == model.current
    in
    Input.button
        [ height <| px n
        , (if curr then
            white

           else
            black
          )
            |> Font.color

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
            [ Font.color View.Style.grey

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
            sand

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
                , View.Style.popIn

                --|> whenAttr (not model.postBeingEdited)
                ]
            |> Element.inFront
            |> whenAttr curr
        , [ Calendar.getDay day.date
                |> Helpers.padNum
                |> text
                |> el
                    [ Element.alignTop
                    , Element.alignLeft
                    , Font.bold
                    , Font.size (n // 2)
                    , abel
                    ]
          , [ icon Icons.assignment_turned_in 15
                |> when (pst |> unwrap False (\p -> not <| List.isEmpty p.tags))
            , icon Icons.edit 15
                |> when (pst |> unwrap False (\p -> isJust p.body))
            ]
                |> row [ spacing 10 ]
          ]
            |> column [ height fill, width fill ]
            |> Element.inFront
        ]
        { onPress = Just <| CellSelect day.date
        , label = none
        }


weekday : String -> Element msg
weekday =
    text >> el [ Font.bold ]


view : Model -> Html Msg
view model =
    let
        frame =
            if model.landscape then
                viewFrame model

            else
                viewFrameMobile model

        wd =
            if model.area < 200000 then
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
            if model.landscape then
                viewHome model

            else
                viewHomeMobile model

        ViewSignup ciph ->
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
                            , btn "Submit" <| SignupSubmit ciph
                            ]
                                |> column
                                    [ cappedWidth 450
                                    , spacing 30
                                    , centerX
                                    , padding 40
                                    ]

                        else
                            [ text "This link is broken."
                            , btn "Re-send email" (NavigateTo Types.RouteHome)
                                |> el [ centerX ]
                            ]
                                |> column
                                    [ centerX
                                    , spacing 20
                                    ]
                    )

        ViewCalendar ->
            (if model.landscape then
                viewPage model

             else
                viewPageMobile model
            )
                |> frame

        ViewSettings ->
            model.auth
                |> unwrap
                    ([ [ text "First day of the week"
                            |> el [ Font.bold, Font.size 22, abel ]
                       , [ Time.Mon
                         , Time.Tue
                         , Time.Wed
                         , Time.Thu
                         , Time.Fri
                         , Time.Sat
                         , Time.Sun
                         ]
                            |> List.map
                                (\d ->
                                    let
                                        curr =
                                            d == model.weekStart
                                    in
                                    { onPress = Just <| WeekdaySet d
                                    , label =
                                        dayShort d
                                            |> (if model.screen.width <= 360 then
                                                    String.left 1

                                                else
                                                    identity
                                               )
                                            |> text
                                    }
                                        |> Input.button
                                            [ Font.color black
                                            , Font.size 17
                                            , Border.shadow
                                                { offset = ( 2, 2 )
                                                , blur = 3
                                                , size = 1
                                                , color = Element.rgb255 150 150 150
                                                }
                                                |> whenAttr curr
                                            , Background.color sand
                                                |> whenAttr curr
                                            , Border.rounded 25
                                            , padding 10
                                            ]
                                )
                            |> row
                                [ if model.screen.width <= 360 then
                                    spaceEvenly

                                  else
                                    spacing 5
                                , width fill
                                ]
                       ]
                        |> column [ width fill, spacing 10 ]
                     , hairline
                     , btn3 False Icons.emoji_events "Sign up now" (NavigateTo Types.RouteHome)
                        |> el [ centerX ]

                     --, hairline
                     --, btn "Load example data" FakeData
                     --|> el [ centerX ]
                     ]
                        |> column [ spacing 20, centerX, cappedWidth 450 ]
                    )
                    (\_ ->
                        [ btn3 model.inProgress.logout Icons.power_off "Logout" Logout
                            |> el [ centerX ]
                        ]
                            |> column [ spacing 20, centerX ]
                    )
                |> frame

        ViewTags ->
            (if model.landscape then
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
            , btn "Continue" (NavigateTo Types.RouteHome)
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
                viewNoTags model

             else
                [ viewTagsCol model tags
                , case model.tagsView of
                    Types.TagsCreate ->
                        [ Input.text
                            [ Border.rounded 0
                            , Border.width 1
                            , width fill
                            , padding 10
                            , onKeydown [ onEnter TagCreateSubmit ]
                            , Html.Attributes.id "editor"
                                |> Element.htmlAttribute
                            ]
                            { onChange = TagCreateNameUpdate
                            , label = Input.labelHidden ""
                            , placeholder =
                                text "New tag"
                                    |> Input.placeholder []
                                    |> Just
                            , text = model.tagCreateName
                            }
                        , [ lnk "Cancel" <| TagsViewSet Types.TagsView
                          , btn2 model.inProgress.tag Icons.send "Submit" TagCreateSubmit
                          ]
                            |> row [ spacing 10, Element.alignRight ]
                        ]
                            |> column [ spacing 10, width fill ]

                    Types.TagsView ->
                        [ [ iBtn 30 Icons.tune <| TagsViewSet Types.TagsSort
                          , [ text "Sorted by"
                            , (case model.tagsSort of
                                Types.SortName ->
                                    "name"

                                Types.SortDate ->
                                    "date"

                                Types.SortUsage ->
                                    "count"
                              )
                                |> text
                            ]
                                |> column [ spacing 5, Font.size 14 ]
                          ]
                            |> row [ spacing 10 ]
                        , iBtn 30 Icons.add <| TagsViewSet Types.TagsCreate
                        ]
                            |> row [ Element.alignBottom, width fill, spaceEvenly ]

                    Types.TagsSort ->
                        [ [ viewSortSelect Types.SortName model.tagsSort
                          , viewSortSelect Types.SortDate model.tagsSort
                          , viewSortSelect Types.SortUsage model.tagsSort
                          ]
                            |> column [ spacing 10 ]
                        , lnk "Cancel" <| TagsViewSet Types.TagsView
                        ]
                            |> column [ spacing 20 ]
                ]
                    |> column
                        [ spacing 10
                        , width fill
                        , height fill
                        ]
            )
            (viewTag model)


viewTagsCol2 : Model -> Date -> List Tag -> List Uuid -> Element Msg
viewTagsCol2 model d tags tagIds =
    tags
        |> (case model.tagsSort of
                Types.SortName ->
                    List.sortBy .name

                Types.SortDate ->
                    List.sortWith
                        (\a b ->
                            DateTime.compare a.created b.created
                        )

                Types.SortUsage ->
                    List.sortBy (.posts >> List.length)
           )
        |> (if model.tagsSortReverse then
                List.reverse

            else
                identity
           )
        |> List.map
            (\t ->
                let
                    curr =
                        List.member t.id tagIds
                in
                Input.button
                    [ Font.size 25
                    , width fill
                    , padding 15
                    , Border.rounded 15
                    , (if curr then
                        white

                       else
                        black
                      )
                        |> Font.color
                    , (if curr then
                        blue

                       else
                        sand
                      )
                        |> Background.color
                    , Border.shadow
                        { offset = ( 3, 3 )
                        , blur = 3
                        , size = 1
                        , color = Element.rgb255 150 150 150
                        }
                    ]
                    { onPress =
                        (if curr then
                            PostTagDetach d t.id

                         else
                            PostTagAttach d t.id
                        )
                            |> Just
                    , label = ellipsisText 26 t.name
                    }
            )
        |> column
            [ spacing 10
            , paddingXY 5 0
            , scrollbarY
            , style "min-height" "auto"
            , width fill
            , height fill
            ]


viewTagsCol : Model -> List Tag -> Element Msg
viewTagsCol model tags =
    tags
        |> (case model.tagsSort of
                Types.SortName ->
                    List.sortBy .name

                Types.SortDate ->
                    List.sortWith
                        (\a b ->
                            DateTime.compare a.created b.created
                        )

                Types.SortUsage ->
                    List.sortBy (.posts >> List.length)
           )
        |> (if model.tagsSortReverse then
                List.reverse

            else
                identity
           )
        |> List.map
            (\t ->
                let
                    curr =
                        model.tag == Just t.id
                in
                Input.button
                    [ Font.size 25
                    , width fill
                    , padding 5
                    ]
                    { onPress = Just <| TagSelect t.id
                    , label =
                        [ [ text t.name
                          , text <| String.fromInt <| List.length t.posts
                          ]
                            |> row
                                [ spaceEvenly
                                , width fill
                                ]
                        , formatDateTime t.created
                            |> text
                            |> el [ Font.size 15, Font.italic ]
                        ]
                            |> column
                                [ spacing 10
                                , width fill
                                , padding 15
                                , Border.rounded 15
                                , (if curr then
                                    white

                                   else
                                    black
                                  )
                                    |> Font.color
                                , (if curr then
                                    blue

                                   else
                                    sand
                                  )
                                    |> Background.color
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


viewNoTags : Model -> Element Msg
viewNoTags model =
    [ Input.text
        [ Border.rounded 0
        , width fill
        , paddingXY 0 10
        , onKeydown [ onEnter TagCreateSubmit ]
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Background.gradient
            { angle = degrees 0
            , steps =
                [ Element.rgb255 245 220 235
                , Element.rgb255 255 255 255
                ]
            }
        ]
        { onChange = TagCreateNameUpdate
        , label = Input.labelHidden ""
        , placeholder =
            text "Create your first tag"
                |> Input.placeholder []
                |> Just
        , text = model.tagCreateName
        }
    , [ [ "Gym"
        , "Nice food"
        , "Flight"
        , "etc."
        ]
            |> List.map (text >> el [ Font.italic ])
            |> column [ spacing 5 ]
      , btn3 model.inProgress.tag Icons.send "Submit" TagCreateSubmit
            |> el [ Element.alignTop ]
      ]
        |> row [ spaceEvenly, width fill ]
    ]
        |> column [ width fill, spacing 10, centerY ]


viewSortIcon : Bool -> Types.TagsSort -> Types.TagsSort -> Element Msg
viewSortIcon rev sort active =
    let
        icn =
            case sort of
                Types.SortName ->
                    Icons.sort_by_alpha

                Types.SortDate ->
                    Icons.date_range

                Types.SortUsage ->
                    Icons.insert_chart_outlined

        curr =
            sort == active
    in
    Input.button
        [ (if curr then
            --blue
            black

           else
            black
          )
            |> Font.color
        , Element.mouseOver [ Font.color blue ]
        ]
        { onPress = Just <| TagsSortSet sort
        , label =
            if curr then
                icon
                    (if rev then
                        Icons.north

                     else
                        Icons.south
                    )
                    30
                    |> el
                        [ Background.color sand
                        , padding 5
                        , Border.rounded 20
                        , Border.shadow
                            { offset = ( 3, 3 )
                            , blur = 3
                            , size = 1
                            , color = Element.rgb255 150 150 150
                            }
                        ]

            else
                icon icn 30
                    |> el
                        [ spacing 10
                        , padding 5
                        ]
        }


viewSortSelect : Types.TagsSort -> Types.TagsSort -> Element Msg
viewSortSelect sort curr =
    let
        txt =
            case sort of
                Types.SortName ->
                    "Name"

                Types.SortDate ->
                    "Date created"

                Types.SortUsage ->
                    "Usage count"

        icn =
            case sort of
                Types.SortName ->
                    Icons.sort_by_alpha

                Types.SortDate ->
                    Icons.date_range

                Types.SortUsage ->
                    Icons.insert_chart_outlined

        active =
            sort == curr
    in
    Input.button
        [ (if active then
            blue

           else
            black
          )
            |> Font.color
        ]
        { onPress = Just <| TagsSortSet sort
        , label = [ icon icn 30, text txt ] |> row [ spacing 10 ]
        }


viewTag : Model -> Tag -> Element Msg
viewTag model t =
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
        , iBtn 30 Icons.close <| TagUpdateSet Nothing
        , iBtn 30 Icons.send <| TagUpdateSubmit t
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
    , if List.isEmpty t.posts then
        viewNoPosts

      else
        t.posts
            |> List.sortWith Calendar.compare
            |> (if model.postSortReverse then
                    List.reverse

                else
                    identity
               )
            |> List.map
                (\date ->
                    Input.button
                        [ Font.size 25
                        , Border.rounded 15
                        , width fill
                        , Background.color sand
                        , Border.shadow
                            { offset = ( 3, 3 )
                            , blur = 3
                            , size = 1
                            , color = Element.rgb255 150 150 150
                            }
                        ]
                        { onPress =
                            Types.RouteDayDetail date
                                |> NavigateTo
                                |> Just
                        , label =
                            [ date |> formatDay |> text

                            --, [ p.body
                            --|> Maybe.withDefault ""
                            --|> text
                            --]
                            --|> paragraph []
                            --|> when False
                            ]
                                |> column
                                    [ spacing 10
                                    , padding 10
                                    , width fill
                                    ]
                        }
                )
            |> column
                [ spacing 20
                , width fill
                , Element.scrollbarY
                , height fill
                , padding 10
                ]
    , [ [ iBtn 30
            (if model.postSortReverse then
                Icons.north

             else
                Icons.south
            )
            PostSortToggle
        , text "Sort"
            |> el [ Font.italic ]
        ]
            |> row [ spacing 10 ]
            |> when (not <| List.isEmpty t.posts)
      , [ btn2 False Icons.delete "Delete" <| TagDelete t
        , iBtn 30 Icons.undo TagDeselect
        ]
            |> row [ spacing 20, Element.alignRight ]
      ]
        |> row [ width fill, spaceEvenly, Element.alignBottom ]
    ]
        |> column [ height fill, width fill, spacing 10 ]


viewNoPosts : Element Msg
viewNoPosts =
    [ text "No days with this tag."
        |> el [ centerX ]
    , btn3
        False
        Icons.calendar_today
        "Go to calendar"
        (NavigateTo Types.RouteCalendar)
        |> el [ centerX ]
    ]
        |> column [ spacing 20, padding 20, centerX ]


viewTags : Model -> Element Msg
viewTags model =
    let
        tags =
            model.tags
                |> UD.values
    in
    if List.isEmpty tags then
        viewNoTags model
            |> el [ cappedWidth 500, centerX, paddingXY 0 20 ]

    else
        [ [ [ [ viewSortIcon model.tagsSortReverse Types.SortName model.tagsSort
              , viewSortIcon model.tagsSortReverse Types.SortDate model.tagsSort
              , viewSortIcon model.tagsSortReverse Types.SortUsage model.tagsSort
              ]
                |> row [ spacing 10 ]
            , (case model.tagsSort of
                Types.SortName ->
                    "name"

                Types.SortDate ->
                    "date created"

                Types.SortUsage ->
                    "count"
              )
                |> (\x ->
                        "Sorted by " ++ x
                   )
                |> text
                |> el [ Font.italic, Font.size 16 ]
            ]
                |> row [ spaceEvenly, width fill, paddingXY 0 10 ]
          , viewTagsCol model tags
          , [ Input.text
                [ Border.rounded 0
                , Border.width 1
                , width <| px 350
                , padding 10
                , onKeydown [ onEnter TagCreateSubmit ]
                , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
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
                |> el [ Element.alignRight ]
            ]
                |> column [ spacing 10, paddingXY 0 20 ]
          ]
            |> column [ cappedWidth 450, centerX, Element.alignTop, height fill ]
        , none |> el [ height fill, width <| px 1, Background.color black ]
        , model.tag
            |> Maybe.andThen
                (\t ->
                    UD.get t model.tags
                )
            |> whenJust
                (\t ->
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
                        , iBtn 30 Icons.close <| TagUpdateSet Nothing
                        , iBtn 30 Icons.send <| TagUpdateSubmit t
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
                    , if List.isEmpty t.posts then
                        viewNoPosts

                      else
                        t.posts
                            |> List.sortWith Calendar.compare
                            |> (if model.postSortReverse then
                                    List.reverse

                                else
                                    identity
                               )
                            |> List.map
                                (\date ->
                                    Input.button
                                        [ Font.size 25
                                        , Border.rounded 15
                                        , width fill
                                        , Background.color sand
                                        , Border.shadow
                                            { offset = ( 3, 3 )
                                            , blur = 3
                                            , size = 1
                                            , color = Element.rgb255 150 150 150
                                            }
                                        ]
                                        { onPress =
                                            Types.RouteDay date
                                                |> NavigateTo
                                                |> Just
                                        , label =
                                            [ date |> formatDay |> text

                                            --, [ p.body
                                            --|> Maybe.withDefault ""
                                            --|> text
                                            --]
                                            --|> paragraph []
                                            --|> when False
                                            ]
                                                |> column
                                                    [ spacing 10
                                                    , padding 10
                                                    , width fill
                                                    ]
                                        }
                                )
                            |> column
                                [ spacing 20
                                , width fill
                                , Element.scrollbarY
                                , height fill
                                , padding 10
                                ]
                    , [ [ iBtn 30
                            (if model.postSortReverse then
                                Icons.north

                             else
                                Icons.south
                            )
                            PostSortToggle
                        , text "Sort"
                            |> el [ Font.italic ]
                        ]
                            |> row [ spacing 10 ]
                            |> when (not <| List.isEmpty t.posts)
                      , [ btn2 False Icons.delete "Delete" <| TagDelete t
                        ]
                            |> row [ spacing 20, Element.alignRight ]
                      ]
                        |> row [ width fill, spaceEvenly, Element.alignBottom ]
                    ]
                        |> column [ height fill, width fill, spacing 10 ]
                )
            |> el [ cappedWidth 500, height fill, centerX, paddingXY 0 20 ]
            |> el [ width fill, height fill ]
        ]
            |> row [ centerX, spacing 30, height fill, width fill ]


viewHomeMobile : Model -> Element Msg
viewHomeMobile model =
    let
        small =
            View.Misc.isSmall model.screen
    in
    [ [ [ text "BOLSTER"
            |> el
                [ (if small then
                    45

                   else
                    65
                  )
                    |> Font.size
                , Font.semiBold
                , abel
                ]
        , View.Img.tmp
            (if small then
                55

             else
                85
            )
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
      , viewInfo small model.def
      ]
        |> column
            [ width fill
            , padding 20
            ]
    , if isNothing model.auth then
        [ viewFunnel model
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
            , Background.color sand
            ]
            { onPress = Just <| NavigateTo Types.RouteCalendar
            , label = text "Try demo"
            }
            |> el [ padding 20, Element.alignRight, Element.alignBottom ]
        ]
            |> column
                [ width fill
                ]

      else
        btn3
            False
            Icons.phonelink
            "Return to app"
            (NavigateTo Types.RouteCalendar)
            |> el [ centerX, centerY ]
    ]
        |> column
            [ height fill
            , width fill
            , spaceEvenly
            , fShrink
            , viewFaq model
                |> Element.inFront

            --, Element.clip
            ]


viewBuy : Model -> Element Msg
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
    [ [ text "Monthly"
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
                    text "$6"
            }
      ]
        |> row [ spaceEvenly, width fill ]
    , [ text "Annual"
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
                    text "$50"
            }
      ]
        |> row [ spaceEvenly, width fill ]
    , [ Element.image
            [ height <| px 35

            --, fadeIn
            , View.Style.popIn
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
            , Background.color sand
            , View.Style.popIn
            , style "transform-origin" "center"
            , shadow2
            , Border.rounded 20
            ]


viewInfo : Bool -> Maybe Def -> Element Msg
viewInfo small mDef =
    [ [ text "The"
            |> el
                [ Element.paddingEach
                    { top = 0
                    , bottom = 9
                    , left = 0
                    , right = 0
                    }
                ]
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
            , (if small then
                17

               else
                20
              )
                |> Font.size
            , varela
            , centerX
            ]
    , mDef
        |> whenJust
            (\d ->
                (case d of
                    Secure ->
                        [ text "Built for performance and security, using the leading technologies available."
                        ]

                    Private ->
                        [ text "Everything you write is end-to-end encrypted, ensuring only you can ever read it." ]

                    Journal ->
                        [ text "For everyday use, on every device." ]
                )
                    |> paragraph
                        [ padding 20

                        --, width Element.shrink
                        , Background.color sand
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
            , Background.color sand
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
    , [ Input.button
            [ centerX
            ]
            { onPress = Just <| NavigateTo Types.RouteCalendar
            , label =
                [ Element.image
                    [ width <| px 200
                    , style "animation-name" "fadeIn"
                    , style "animation-duration" "1s"
                    ]
                    { src = "/phone.png"
                    , description = ""
                    }
                , btn3
                    False
                    Icons.phonelink
                    (if isNothing model.auth then
                        "Try demo"

                     else
                        "Return to app"
                    )
                    (NavigateTo Types.RouteCalendar)
                    |> el [ centerX ]
                ]
                    |> column [ spacing 10 ]
            }
      , [ viewInfo False model.def
            |> el
                [ width <| px 420
                , Element.alignTop
                ]
        , viewFunnel model
            |> when (isNothing model.auth)
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
        , Background.color sand
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
                , Font.size 24
                , ebg
                , Font.italic
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
                    --sand
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
            , btn "Re-send email" (NavigateTo Types.RouteHome)
                |> el [ centerX ]
            ]
                |> column
                    [ centerX
                    , spacing 20
                    ]

        JoinUs ->
            viewBuy model
                |> el [ paddingXY 20 0, width fill ]


viewWelcome : Model -> String -> Element Msg
viewWelcome model nonce =
    [ text "Welcome back"
        |> el [ Font.italic, Font.bold, Font.size 28, abel ]
    , [ Input.currentPassword
            [ Border.rounded 0
            , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }

            --, Border.width 0
            --, height <| px 50
            , paddingXY 0 10

            --, style "cursor" "wait"
            --|> whenAttr model.inProgress
            , width fill
            , onKeydown [ onEnter <| LoginSubmit nonce ]

            --|> whenAttr valid
            ]
            { onChange = LoginFormPasswordUpdate
            , label = Input.labelHidden ""
            , placeholder =
                text "Your password"
                    |> el [ centerY ]
                    |> Input.placeholder []
                    |> Just
            , text = model.loginForm.password
            , show = False
            }
      , [ lnk "Back" Change
        , btn3 model.inProgress.login Icons.save "Submit" (LoginSubmit nonce)
        ]
            |> row [ Element.alignRight, spacing 10 ]
      ]
        |> column
            [ width fill
            , spacing 10
            ]
    ]
        |> column
            [ width fill
            , spacing 10
            , paddingXY 20 0
            , fadeIn
            ]


viewFrame : Model -> Element Msg -> Element Msg
viewFrame model elem =
    let
        wd =
            if model.area < 200000 then
                10

            else
                20
    in
    [ [ Input.button
            [ Font.semiBold
            ]
            { onPress = Just <| NavigateTo Types.RouteHome
            , label =
                [ View.Img.tmp 40
                    |> Element.html
                    |> el []
                , text "BOLSTER" |> el [ abel, Font.size 30 ]
                , text "DEMO"
                    |> el [ Font.light, varela ]
                    |> when (model.auth == Nothing)
                ]
                    |> row [ spacing 10 ]
            }
      , [ ( "Calendar", Types.RouteCalendar, ViewCalendar )
        , ( "Tags", Types.RouteTags, ViewTags )
        , ( "Settings", Types.RouteSettings, ViewSettings )
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
            if model.area < 200000 then
                10

            else
                20

        pic =
            if model.area < 200000 then
                35

            else
                50
    in
    [ [ Input.button []
            { onPress = Just <| NavigateTo Types.RouteHome
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
              , [ viewRoute Types.RouteCalendar model.view
                , viewRoute Types.RouteTags model.view
                , viewRoute Types.RouteSettings model.view
                ]
                    |> column [ spacing 10, padding 10 ]
                    |> when model.dropdown
              ]
                |> column
                    [ Background.color sand
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
    , viewBottomBar model
        |> when (model.screen.height >= View.Misc.tallInt)
    ]
        |> column
            --[ spacing nd
            [ spaceEvenly
            , height fill
            , width fill

            --, Element.clip
            , fShrink
            , padding nd
            ]


viewRoute : Route -> View -> Element Msg
viewRoute r v =
    let
        txt =
            case r of
                Types.RouteCalendar ->
                    "Calendar"

                Types.RouteTags ->
                    "Tags"

                Types.RouteSettings ->
                    "Settings"

                _ ->
                    "???"

        icn =
            case r of
                Types.RouteCalendar ->
                    Icons.calendar_today

                Types.RouteTags ->
                    Icons.assignment_turned_in

                Types.RouteSettings ->
                    Icons.settings

                _ ->
                    Icons.help

        active =
            case r of
                Types.RouteCalendar ->
                    v == ViewCalendar

                Types.RouteTags ->
                    v == ViewTags

                Types.RouteSettings ->
                    v == ViewSettings

                _ ->
                    False
    in
    Input.button
        [ Border.width 1 |> whenAttr active
        , Element.mouseOver
            [ Font.color blue
            ]
        , Font.size 25
        , width fill
        , padding 10
        ]
        { onPress =
            (if active then
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


viewBottomBar : Model -> Element Msg
viewBottomBar model =
    let
        iconSize =
            if model.area < 200000 then
                30

            else if model.area < 300000 then
                40

            else
                50
    in
    [ ( Icons.settings, Types.RouteSettings, ViewSettings )
    , ( Icons.event, Types.RouteCalendar, ViewCalendar )

    -- note_add
    , ( Icons.assignment_turned_in, Types.RouteTags, ViewTags )
    ]
        |> List.map
            (\( n, r, v ) ->
                let
                    curr =
                        v == model.view

                    col =
                        case r of
                            Types.RouteSettings ->
                                View.Style.gold

                            Types.RouteCalendar ->
                                blue

                            Types.RouteTags ->
                                View.Style.red

                            _ ->
                                View.Style.garish
                in
                Input.button
                    [ Font.underline |> whenAttr curr
                    , width fill
                    , (if curr then
                        white

                       else
                        black
                      )
                        |> Font.color
                    , height <| px <| iconSize + 15
                    ]
                    { onPress =
                        if curr then
                            Nothing

                        else
                            Just <| NavigateTo r
                    , label =
                        none
                            |> el
                                [ icon n iconSize
                                    |> el
                                        [ centerX
                                        , centerY
                                        ]
                                    |> Element.inFront
                                , none
                                    |> el
                                        [ width <| px <| iconSize + 15
                                        , height <| px <| iconSize + 15
                                        , Background.color col
                                        , style "transform-origin" "center"
                                        , View.Style.popIn
                                        , Border.rounded 50
                                        , centerX
                                        , centerY
                                        ]
                                    |> Element.behindContent
                                    |> whenAttr curr
                                , centerX
                                , height fill
                                ]
                    }
            )
        |> row [ Element.alignBottom, width fill ]


viewPage : Model -> Element Msg
viewPage model =
    let
        wd =
            if model.area < 200000 then
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


viewPageMobile : Model -> Element Msg
viewPageMobile model =
    if model.postView then
        model.current
            |> whenJust (viewPostView model)

    else
        [ viewCalendar model
            |> el
                [ Element.alignTop
                , width fill

                --, style "transform-origin" "bottom center"
                --, style "animation-fill-mode" "forwards"
                --, style "animation" "fadeOut 0.5s"
                ]
        , model.current
            |> unwrap
                (viewTodayBtn model.screen
                    |> el [ centerX, centerY ]
                )
                (viewBarMobile model)
        ]
            |> column
                [ width fill
                , height fill
                , spaceEvenly

                --, paddingXY 20 10
                --, Element.clip
                , fShrink
                ]


viewPostView : Model -> Date -> Element Msg
viewPostView model d =
    let
        pst =
            model.posts
                |> Day.get d

        fs =
            25

        txt =
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

        dayTxt =
            formatDay d
                |> text
                |> el [ width fill ]
    in
    if model.tagView then
        [ [ dayTxt
          , text "|"
          , text "Tags"
                |> el [ Element.alignRight ]
                |> el [ Font.italic, width fill ]
          ]
            |> row [ spaceEvenly, Font.size 17, width fill ]
        , viewPostTags model d pst
        , [ btn2 False Icons.edit "Write" <| NavigateTo <| Types.RouteDayDetail d
          , iBtn 30 Icons.expand_more <| NavigateTo Types.RouteCalendar
          ]
            |> row [ width fill, spaceEvenly, alignBottom, width fill ]
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

    else
        [ [ dayTxt
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
        , viewPostEditor
            model.postEditorBody
            (not model.postBeingEdited)
            fs
        , if model.postBeingEdited then
            [ lnk "Cancel" PostUpdateCancel
            , btn2 model.inProgress.post
                Icons.save
                "Submit"
                PostBodySubmit
            ]
                |> row [ spacing 20, Element.alignRight ]

          else
            [ btn2 False Icons.assignment_turned_in "Tags" <| NavigateTo <| Types.RouteDayTags d
            , btn2 False Icons.edit "Edit" PostUpdateStart
            , iBtn 30 Icons.expand_more <| NavigateTo Types.RouteCalendar
            ]
                |> row [ width fill, spaceEvenly, alignBottom, width fill ]
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


viewBarMobile : Model -> Date -> Element Msg
viewBarMobile model day =
    let
        pst =
            model.posts
                |> Day.get day

        body =
            pst
                |> Maybe.andThen .body
                |> Maybe.withDefault ""
    in
    [ viewTodayBtn model.screen
        |> el [ centerX, Element.alignBottom ]
        |> when (day /= model.today || model.month /= Calendar.getMonth day)
        |> el [ width fill, height fill ]
    , [ Input.button [ height fill, width fill ]
            { onPress = Just <| NavigateTo <| Types.RouteDayDetail day
            , label =
                [ viewPreview body
                , icon Icons.edit 20
                    |> el [ Element.alignTop ]
                ]
                    |> row [ spacing 10, height fill, width fill ]
            }
      , Input.button [ width fill ]
            { onPress = Just <| NavigateTo <| Types.RouteDayTags day
            , label =
                [ pst
                    |> unwrap 0
                        (.tags >> List.length)
                    |> String.fromInt
                    |> text
                , icon Icons.assignment_turned_in 20
                ]
                    |> row [ spacing 10, Element.alignRight ]
            }

      --, View.Misc.dayParts day
      --|> List.map (text >> el [ centerX ] >> el [ width fill ])
      --|> row [ width fill, spaceEvenly, Font.size 16 ]
      , formatDay day
            |> text
            |> el
                [ (if model.screen.width < 360 then
                    14

                   else
                    16
                  )
                    |> Font.size
                , Font.italic
                , Element.alignRight
                ]
      ]
        |> column
            [ height fill
            , width fill
            , spacing 10
            ]
        |> el
            [ Border.widthEach { bottom = 0, top = 0, left = 1, right = 0 }
            , paddingXY 5 0
            , width fill
            , height fill
            ]
    ]
        |> row
            [ spacing 10
            , Helpers.View.cappedHeight 175
            , paddingXY 0 10
            , width fill
            ]


viewTodayBtn : Types.Screen -> Element Msg
viewTodayBtn screen =
    Input.button
        [ Element.paddingXY 15 0
        , Font.color black
        , height <| px 50
        , Border.rounded 25
        , Background.color sand
        , varela
        , Border.shadow
            { offset = ( 3, 3 )
            , blur = 3
            , size = 0
            , color = Element.rgb255 150 150 150
            }
        , Font.size 17
        , View.Style.popIn
        ]
        { onPress = Just <| GoToToday Nothing
        , label =
            [ icon Icons.brightness_5 20
            , (if screen.width < 360 then
                "Today"

               else
                "Go to today"
              )
                |> text
            ]
                |> row [ spacing 10 ]
        }


viewReady : Element Msg
viewReady =
    Input.button
        [ width fill
        , Helpers.View.cappedHeight 700
        , Background.color sand
        , style "cursor" View.Img.pencil
        ]
        { onPress = Just <| ReadyStart Nothing
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

        tags =
            UD.values model.tags

        body =
            if model.postBeingEdited then
                model.postEditorBody

            else
                pst
                    |> Maybe.andThen .body
                    |> Maybe.withDefault ""

        topBar =
            [ formatDay d
                |> text
                |> el [ Font.size 30, abel, Font.color white, Background.color black, padding 10 ]
            , [ lnk "Cancel" PostUpdateCancel
                    |> when model.postBeingEdited
              , if model.postBeingEdited then
                    pst
                        |> unwrap
                            (btn2 model.inProgress.post
                                Icons.save
                                "Save"
                                PostBodySubmit
                                |> when (model.postEditorBody /= "")
                            )
                            (\p ->
                                btn2 model.inProgress.post
                                    Icons.save
                                    "Save"
                                    PostBodySubmit
                                    |> when (p.body /= Just model.postEditorBody)
                            )

                else
                    btn2 False
                        Icons.edit
                        "Edit"
                        PostUpdateStart
              ]
                |> row [ spacing 20 ]
            ]
                |> row
                    [ spaceEvenly
                    , Font.size 17
                    , width fill
                    ]
    in
    [ [ topBar
      , viewPostEditor body (not model.postBeingEdited) fs
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
    , [ [ viewSortIcon model.tagsSortReverse Types.SortName model.tagsSort
        , viewSortIcon model.tagsSortReverse Types.SortDate model.tagsSort
        , viewSortIcon model.tagsSortReverse Types.SortUsage model.tagsSort
        ]
            |> row [ spaceEvenly, paddingXY 0 10, width fill ]
      , viewTagsCol2 model
            d
            tags
            (pst |> unwrap [] (.tags >> List.map .tag))
      ]
        |> column [ height fill, paddingXY 20 0, width <| px 200 ]
        |> when (not <| List.isEmpty tags)
    ]
        |> row [ height fill, width fill ]


viewPostEditor : String -> Bool -> Int -> Element Msg
viewPostEditor txt disable fontSize =
    Html.textarea
        [ Html.Attributes.id "editor"
        , Html.Attributes.value txt
        , Html.Attributes.style "font-size" "inherit"
        , Html.Attributes.style "font-family" "inherit"
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
                View.Img.pencil

               else
                "text"
              )
                |> style "cursor"
            , height fill
            , Background.color sand
            , Font.size fontSize
            , padding 10
            , ebg
            , Element.Events.onClick PostUpdateStart
                |> whenAttr disable
            , onKeydown [ onCtrlEnter PostBodySubmit ]
            ]


viewPreview : String -> Element Msg
viewPreview txt =
    Html.textarea
        [ Html.Attributes.id "editor"
        , Html.Attributes.value txt
        , Html.Attributes.style "font-size" "inherit"
        , Html.Attributes.style "font-family" "inherit"
        , Html.Attributes.style "font-color" "inherit"
        , Html.Attributes.style "padding" "0px"
        , Html.Attributes.style "flex-grow" "inherit"
        , Html.Attributes.style "height" "inherit"
        , Html.Attributes.style "min-height" "auto"
        , Html.Attributes.readonly True
        , Html.Attributes.style "overflow" "hidden"
        ]
        []
        |> Element.html
        |> el
            [ width fill
            , height fill
            , Background.color sand
            , Font.color black
            , Font.size 14
            , padding 5
            , ebg
            , el
                [ Helpers.View.cappedHeight 35
                , width fill
                , Element.alignBottom
                , Background.gradient
                    { angle = degrees 0
                    , steps =
                        [ Element.rgba255 255 245 235 1
                        , Element.rgba255 255 245 235 0.8
                        , Element.rgba255 255 245 235 0.7
                        , Element.rgba255 255 245 235 0.5
                        , Element.rgba255 255 245 235 0.4
                        ]
                    }
                ]
                none
                |> Element.inFront
            ]


viewPostTags : Model -> Date -> Maybe Post -> Element Msg
viewPostTags model d pst =
    let
        xs =
            UD.values model.tags

        postTagIds =
            pst |> unwrap [] (.tags >> List.map .tag)
    in
    if List.isEmpty xs then
        [ [ text "You don't have any tags." ]
            |> paragraph []
        , btn3
            False
            Icons.assignment_turned_in
            "Go to tags"
            (NavigateTo Types.RouteTags)
            |> el [ centerX ]
        ]
            |> column [ spacing 20, padding 20, centerX, centerY ]

    else
        xs
            |> List.map
                (\t ->
                    let
                        flip =
                            List.member t.id postTagIds

                        prog =
                            List.member ( d, t.id ) model.inProgress.postTags
                    in
                    --[ ellipsisText 20 t.name
                    [ Input.button [ width fill ]
                        { onPress = Just <| TagSelect t.id
                        , label = paragraph [] [ text t.name ]
                        }
                    , Input.button
                        [ width <| px 40
                        , height <| px 40
                        , Border.width 1
                        , (if prog then
                            sand

                           else
                            white
                          )
                            |> Background.color
                        ]
                        { onPress =
                            if prog then
                                Nothing

                            else if flip then
                                Just <| PostTagDetach d t.id

                            else
                                Just <| PostTagAttach d t.id
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


{-| To handle scrollbarY problems.
<https://github.com/mdgriffith/elm-ui/issues/149>
-}
fShrink : Attribute msg
fShrink =
    style "flex-shrink" "1"
        |> whenAttr False


hairline : Element msg
hairline =
    el [ height <| px 1, width fill, Background.color black ] none
