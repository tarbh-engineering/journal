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
import Material.Icons.Types exposing (Icon)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Time exposing (Month(..))
import Time.Format.I18n.I_en_us exposing (dayShort, monthName)
import Types exposing (Def(..), Funnel(..), Model, Msg(..), Post, Tag, View(..))
import View.Img
import View.Misc exposing (btn, btn2, btn3, formatDateTime, formatDay, iBtn, icon, lnk, spinner)
import View.Style exposing (abel, black, blue, ebg, fadeIn, grey, popIn, red, rotate, sand, serif, white)


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
        , color = grey
        }


shadow3 : Attribute msg
shadow3 =
    Border.shadow
        { offset = ( 3, 3 )
        , blur = 3
        , size = 0
        , color = grey
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
                15

            else
                25
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
                , Font.size 25
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

        col =
            if model.month == Calendar.getMonth day.date then
                black

            else
                grey

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
        , Element.mouseOver
            [ Font.color View.Style.grey
            ]
            |> whenAttr (not model.isMobile && not curr)
        , (if day.month == EQ then
            sand

           else
            white
          )
            |> Background.color
        , none
            |> el
                [ width fill
                , height fill
                , Background.color col
                , style "transform-origin" "center"
                , popIn
                ]
            |> Element.inFront
            |> whenAttr curr
        , [ [ Calendar.getDay day.date
                |> Helpers.padNum
                |> text
                |> el
                    [ Element.alignTop
                    , Element.alignLeft
                    , Font.bold
                    , Font.size (n // 2)
                    , abel
                    ]
                |> el [ width fill, height fill ]
            , icon Icons.brightness_5 20
                |> when (model.today == day.date)
                |> el [ centerX ]
                |> el [ width fill ]
            ]
                |> row [ width fill, height fill ]
          , [ icon Icons.edit 20
                |> when (pst |> unwrap False (\p -> isJust p.body))
                |> el [ centerX ]
                |> el [ width fill ]
            , icon Icons.assignment_turned_in 20
                |> when (pst |> unwrap False (\p -> not <| List.isEmpty p.tags))
                |> el [ centerX ]
                |> el [ width fill ]
            ]
                |> row [ width fill, height fill ]
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
                                                , color = grey
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
    ]
        |> column
            [ spacing wd
            , height fill
            , width fill
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
                        Just red
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
         , View.Style.baseFont
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
                                serif "New tag"
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
                        , color = grey
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
                                    , color = grey
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
        , Font.size 25
        ]
        { onChange = TagCreateNameUpdate
        , label = Input.labelHidden ""
        , placeholder =
            serif "Create your first tag"
                |> Input.placeholder []
                |> Just
        , text = model.tagCreateName
        }
    , [ [ "Restaurant"
        , "Flight"
        , "Gym"
        , "etc."
        ]
            |> List.map (text >> el [ Font.italic ])
            |> column [ spacing 5, Font.size 20 ]
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
        [ Font.color black
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
                            , color = grey
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
                            , color = grey
                            }
                        ]
                        { onPress =
                            Types.RouteDayDetail date
                                |> NavigateTo
                                |> Just
                        , label =
                            date
                                |> formatDay
                                |> text
                                |> el
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
                    serif "New tag"
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
                                            , color = grey
                                            }
                                        ]
                                        { onPress =
                                            Types.RouteDay date
                                                |> NavigateTo
                                                |> Just
                                        , label =
                                            date
                                                |> formatDay
                                                |> text
                                                |> el
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
    [ [ [ [ View.Img.loci 65
                |> Element.html
                |> el []
          , text "BOLSTER"
                |> el
                    [ (if small then
                        35

                       else
                        55
                      )
                        |> Font.size
                    , Font.semiBold
                    , abel
                    ]
          ]
            |> row
                [ spacing 20
                , centerX
                ]
        , serif "The secure, private journal."
            |> el
                [ Font.size 20
                , centerX
                ]
        ]
            |> column
                [ width fill
                , spacing 10
                ]
      , viewInfo model.def
      ]
        |> column
            [ width fill
            , spacing 30
            ]
    , if isNothing model.auth then
        viewFunnel model

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
            , padding 20
            ]


viewInfo : Maybe Def -> Element Msg
viewInfo def =
    [ [ viewClick def Private
      , viewClick def Control
      , viewClick def Devices
      , viewClick def OpenSource
      ]
        |> column [ spacing 10 ]
    , def
        |> unwrap
            ([ viewLabel Private
             , viewLabel Control
             , viewLabel Devices
             , viewLabel OpenSource
             ]
                |> column [ spacing 10 ]
            )
            (\d ->
                [ Input.button
                    [ height <| px 40, paddingXY 10 0, Font.bold ]
                    { onPress = Just <| SetDef d
                    , label =
                        defTitle d
                            |> text
                            |> el [ centerY ]
                    }
                , defText d
                    |> column [ spacing 10, height fill, paddingXY 10 0 ]
                ]
                    |> column
                        [ width fill
                        , spacing 5
                        , Element.alignTop
                        , height fill
                        , Background.color sand
                        , Border.roundEach
                            { topLeft = 0
                            , bottomRight = 0
                            , topRight = 0
                            , bottomLeft =
                                if d == Types.OpenSource then
                                    0

                                else
                                    25
                            }
                        , shadow3
                        ]
            )
    ]
        |> row [ width fill, Font.size 17 ]


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
                , color = grey
                }
            , Element.mouseDown
                [ Element.moveRight 5
                , Element.moveDown 5
                , Border.shadow
                    { offset = ( 0, 0 )
                    , blur = 0
                    , size = 0
                    , color = grey
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
                , color = grey
                }
            , Element.mouseDown
                [ Element.moveRight 5
                , Element.moveDown 5
                , Border.shadow
                    { offset = ( 0, 0 )
                    , blur = 0
                    , size = 0
                    , color = grey
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
    , [ Element.newTabLink []
            { url = "https://stripe.com"
            , label = View.Img.stripe |> Element.html
            }
      , lnk "Back" FunnelCancel
      ]
        |> row [ spaceEvenly, width fill ]
    ]
        |> column
            [ Element.alignRight
            , spacing 20
            , cappedWidth 450
            , padding 20
            , Element.alignBottom
            , Background.color sand
            , popIn
            , style "transform-origin" "center"
            , shadow2
            , Border.rounded 20
            ]


viewHome : Model -> Element Msg
viewHome model =
    [ [ View.Img.dark
            |> Element.html
            |> el
                [ height <| px 250
                , width <| px 250
                , Background.color black
                , padding 30
                ]
      , [ text "BOLSTER"
            |> el
                [ Font.size 120
                , Font.semiBold
                , abel
                , paddingXY 20 0
                ]
        , el [ Background.color black, width fill, height <| px 5 ] none
        , serif "The secure, private journal."
            |> el [ Font.size 35, centerX ]
        ]
            |> column [ spacing 10 ]
      ]
        |> row
            [ style "animation-name" "fadeIn"
            , style "animation-duration" "1s"
            , centerX
            ]
    , [ [ [ viewLn model.def Private 0.25
          , viewLn model.def Control 0.5
          , viewLn model.def Devices 0.75
          , viewLn model.def OpenSource 1.0
          ]
            |> column [ spacing 10, Element.alignLeft ]
        , model.def
            |> whenJust
                (\d ->
                    defText d
                        |> column
                            [ padding 20
                            , spacing 20
                            , Background.color sand
                            , ebg
                            , Font.size 28
                            , width fill
                            , height fill
                            , Border.shadow
                                { offset = ( 0, 3 )
                                , blur = 0
                                , size = 0
                                , color = grey
                                }
                            ]
                )
        ]
            |> row [ centerX, width <| px 750 ]
      , if isNothing model.auth then
            viewFunnel model

        else
            btn3
                False
                Icons.phonelink
                "Return to app"
                (NavigateTo Types.RouteCalendar)
                |> el [ centerX ]
      ]
        |> column [ spacing 50, height fill, centerX ]
    ]
        |> column
            [ spacing 50
            , centerX
            , padding 30
            , height fill
            ]


defText : Def -> List (Element msg)
defText d =
    case d of
        OpenSource ->
            [ [ text "Built for performance and security, using the leading technologies available." ]
                |> paragraph []
            , [ text "The code can be viewed "
              , Element.newTabLink
                    [ Font.underline
                    , Element.mouseOver
                        [ Font.color blue
                        ]
                    ]
                    { url = "https://github.com/tarbh-engineering/journal"
                    , label = text "here"
                    }
              , text "."
              ]
                |> paragraph []
            ]

        Private ->
            [ [ text "Everything you write is encrypted on your device before it is saved, ensuring only you can ever read it." ]
                |> paragraph []
            ]

        Control ->
            [ [ text "Export your data in a variety of formats at any time." ]
                |> paragraph []
            ]

        Devices ->
            [ [ text "For everyday use on mobile, desktop and tablet." ]
                |> paragraph []
            , [ text "Visit this website using your devices to install for "
              , Element.newTabLink
                    [ Font.underline
                    , Element.mouseOver
                        [ Font.color blue
                        ]
                    ]
                    { url = "https://mobilesyrup.com/2020/05/24/how-install-progressive-web-app-pwa-android-ios-pc-mac/"
                    , label = text "iOS and Android"
                    }
              , text "."
              ]
                |> paragraph []
            ]


viewClick : Maybe Def -> Def -> Element Msg
viewClick c def =
    let
        icn =
            defIcon def

        curr =
            c == Just def
    in
    Input.button
        [ Background.color sand
            |> whenAttr curr
        , Border.roundEach { topLeft = 0, bottomRight = 0, topRight = 0, bottomLeft = 25 }
        , shadow3
            |> whenAttr curr
        , centerY
        , Element.paddingEach { top = 10, bottom = 10, left = 10, right = 5 }
        ]
        { onPress = Just <| SetDef def
        , label = icon icn 20
        }


viewLabel : Def -> Element Msg
viewLabel def =
    Input.button
        [ width fill
        , height <| px 40
        , Font.underline
        , style "text-decoration-style" "dashed"
        , paddingXY 10 0
        ]
        { onPress = Just <| SetDef def
        , label =
            defTitle def
                |> text
                |> el
                    [ centerY
                    ]
        }


defTitle : Def -> String
defTitle def =
    case def of
        Private ->
            "End-to-end encrypted privacy"

        Devices ->
            "For use on every device"

        OpenSource ->
            "Open source codebase"

        Control ->
            "Full control over your data"


defIcon : Def -> Icon msg
defIcon d =
    case d of
        Private ->
            Icons.lock

        Control ->
            Icons.save

        Devices ->
            Icons.devices

        OpenSource ->
            Icons.public


viewLn : Maybe Def -> Def -> Float -> Element Msg
viewLn c def fl =
    let
        icn =
            defIcon def

        curr =
            c == Just def
    in
    Input.button
        [ Background.color sand |> whenAttr curr
        , width fill
        , Border.shadow
            { offset = ( 0, 3 )
            , blur = 0
            , size = 0
            , color = grey
            }
            |> whenAttr curr
        , Font.bold
            |> whenAttr curr
        , padding 10
        , Element.mouseOver [ Font.color blue ]
        , Element.transparent True
        , style "animation-name" "fadeIn"
        , style "animation-duration" "1s"
        , style "animation-fill-mode" "forwards"
        , style "animation-delay" (String.fromFloat fl ++ "s")
        ]
        { onPress = Just <| SetDef def
        , label =
            [ icon icn 30
                |> el
                    [ padding 10
                    , Background.color sand
                        |> whenAttr (not curr)
                    , Border.rounded 25
                    , shadow3
                        |> whenAttr (not curr)
                    ]
            , defTitle def
                |> text
                |> el
                    [ Font.size 20
                    , paddingXY 10 0
                    ]
            ]
                |> row [ spacing 5 ]
        }


viewFunnel : Model -> Element Msg
viewFunnel model =
    let
        ent =
            if model.funnel == Hello then
                EmailSubmit

            else
                FunnelCancel
    in
    case model.funnel of
        Hello ->
            [ [ Input.email
                    [ Border.rounded 0
                    , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
                    , paddingXY 0 10
                    , width fill
                    , onKeydown [ onEnter ent ]
                    , Font.size 24
                    , ebg
                    , Font.italic
                    ]
                    { onChange = LoginFormEmailUpdate
                    , label = Input.labelHidden ""
                    , placeholder =
                        serif "Your email address"
                            |> Input.placeholder []
                            |> Just
                    , text = model.loginForm.email
                    }
              , btn2 model.inProgress.login Icons.send "Submit" ent
                    |> el [ Element.alignRight ]
              ]
                |> column
                    [ spacing 10
                    , Element.alignRight
                    , cappedWidth 450
                    ]
            , btn3
                False
                Icons.phonelink
                "Try the demo"
                (NavigateTo Types.RouteCalendar)
                |> el [ centerX ]
            ]
                |> column
                    [ spacing 40
                    , width fill
                    ]

        PayErr ->
            [ [ text "The payment process was not completed." ]
                |> paragraph [ Font.center ]
            , lnk "Continue" FunnelCancel
                |> el [ centerX ]
            ]
                |> column
                    [ padding 20
                    , spacing 20
                    , Background.color sand
                    , cappedWidth 450
                    , shadow3
                    , Border.rounded 25
                    , Element.alignRight
                    , popIn
                    ]

        PayOk ->
            [ text "Thank you for your purchase."
                |> el [ centerX, Font.bold ]
            , [ text "Please check your email inbox to proceed." ]
                |> paragraph [ Font.center ]
            , lnk "Close" FunnelCancel
                |> el [ centerX ]
            ]
                |> column
                    [ padding 20
                    , spacing 20
                    , Background.color sand
                    , cappedWidth 450
                    , shadow3
                    , Border.rounded 25
                    , Element.alignRight
                    , popIn
                    ]

        Signup ciph ->
            model.magic
                |> whenJust
                    (\b ->
                        if b then
                            viewSignup model (SignupSubmit ciph)

                        else
                            [ text "This link is broken."
                            , btn "Continue" FunnelCancel
                                |> el [ centerX ]
                            ]
                                |> column
                                    [ centerX
                                    , spacing 20
                                    ]
                    )

        WelcomeBack nonce ->
            viewWelcome model nonce

        JoinUs ->
            viewBuy model

        GuestSignup x ->
            viewSignup model (GuestSignupSubmit x)


viewSignup : Model -> Msg -> Element Msg
viewSignup model msg =
    [ text "Welcome"
        |> el [ Font.bold ]
    , text "Choose a password to protect your account"
        |> el [ Font.italic ]
    , Input.currentPassword
        [ Border.rounded 0
        , Border.width 1
        , width fill
        , Border.widthEach
            { top = 0
            , bottom = 1
            , left = 0
            , right = 0
            }
        , paddingXY 0 10
        ]
        { onChange = LoginFormPasswordUpdate
        , label = Input.labelHidden ""
        , show = False
        , placeholder =
            serif "Your password"
                |> Input.placeholder []
                |> Just
        , text = model.loginForm.password
        }
    , [ lnk "Cancel" FunnelCancel
      , btn3 model.inProgress.login Icons.send "Submit" msg
      ]
        |> row [ Element.alignRight, spacing 20 ]
    ]
        |> column
            [ cappedWidth 450
            , spacing 20
            , Element.alignRight
            ]


viewWelcome : Model -> String -> Element Msg
viewWelcome model nonce =
    [ text "Welcome back"
        |> el [ Font.bold, Font.size 28 ]
    , [ Input.currentPassword
            [ Border.rounded 0
            , Border.widthEach { top = 0, bottom = 1, left = 0, right = 0 }
            , paddingXY 0 10
            , width fill
            , onKeydown [ onEnter <| LoginSubmit nonce ]
            ]
            { onChange = LoginFormPasswordUpdate
            , label = Input.labelHidden ""
            , placeholder =
                serif "Your password"
                    |> el [ centerY ]
                    |> Input.placeholder []
                    |> Just
            , text = model.loginForm.password
            , show = False
            }
      , [ lnk "Back" FunnelCancel
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
            [ cappedWidth 450
            , Element.alignRight
            , spacing 10
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
                [ View.Img.loci 40
                    |> Element.html
                    |> el []
                , text "BOLSTER" |> el [ abel, Font.size 30 ]
                , serif "DEMO"
                    |> el [ Font.light ]
                    |> when (model.auth == Nothing)
                ]
                    |> row [ spacing 10 ]
            }
      , [ viewNavButton Icons.event "Calendar" Types.RouteCalendar (model.view == ViewCalendar)
        , viewNavButton Icons.assignment_turned_in "Tags" Types.RouteTags (model.view == ViewTags)
        , viewNavButton Icons.settings "Settings" Types.RouteSettings (model.view == ViewSettings)
        ]
            |> row [ spacing 40 ]
      ]
        |> row [ width fill, spaceEvenly, paddingXY 20 wd ]
    , elem
    ]
        |> column [ spacing wd, height fill, cappedWidth 1275, centerX ]


viewNavButton : Icon Msg -> String -> Types.Route -> Bool -> Element Msg
viewNavButton icn n r curr =
    Input.button
        [ padding 10
        , (if curr then
            sand

           else
            white
          )
            |> Background.color
        , shadow3
            |> whenAttr curr
        , Element.mouseOver [ Font.color blue ]
            |> whenAttr (not curr)
        , Border.roundEach
            { topLeft = 0
            , bottomRight = 0
            , topRight = 25
            , bottomLeft = 25
            }
        ]
        { onPress =
            if curr then
                Nothing

            else
                Just <| NavigateTo r
        , label =
            [ icon icn 30, text n ]
                |> row [ spacing 10 ]
        }


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
                25

            else
                40
    in
    [ [ Input.button []
            { onPress = Just <| NavigateTo Types.RouteHome
            , label =
                [ View.Img.loci pic
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
      ]
        |> row
            [ width fill
            , spaceEvenly
            ]
    , elem
    , viewBottomBar model
        |> when (model.screen.height >= View.Misc.tallInt)
    ]
        |> column
            [ spaceEvenly
            , height fill
            , width fill
            , fShrink
            , paddingXY nd 10
            ]


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
                                        , popIn
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
    [ [ viewCalendar model
            |> el
                [ Element.alignTop
                ]
      , viewTodayBtn model.screen
            |> el
                [ Element.alignBottom
                , Element.alignRight
                , padding 10
                ]
            |> when
                (model.current
                    /= Just model.today
                    || (model.current
                            |> unwrap False (\c -> model.month /= Calendar.getMonth c)
                       )
                )
      ]
        |> column [ height fill ]
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
        , Border.roundEach { topLeft = 0, bottomRight = 0, topRight = 25, bottomLeft = 25 }
        , Background.color sand
        , shadow3
        , Font.size 17
        , popIn
        , Element.mouseOver
            [ Background.color black
            , Font.color white
            ]
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
        , height fill
        , Background.color View.Style.paper
        , style "cursor" View.Img.pencil
        , shadow3
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
            , Background.color View.Style.paper
            , Font.size fontSize
            , padding 10
            , ebg
            , Element.Events.onClick PostUpdateStart
                |> whenAttr disable
            , onKeydown [ onCtrlEnter PostBodySubmit ]
            , shadow3
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
