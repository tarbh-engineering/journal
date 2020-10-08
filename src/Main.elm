module Main exposing (main)

import Browser
import Browser.Events
import Calendar
import Data
import Day
import Derberos.Date.Utils exposing (numberToMonth)
import Helpers
import Helpers.UuidDict as UD
import Json.Decode as JD
import Maybe.Extra exposing (unwrap)
import Ports
import Routing
import Task
import Time
import Types exposing (Flags, Model, Msg, Screen)
import Update exposing (update)
import Url
import View exposing (view)
import View.Misc


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        url =
            Url.fromString flags.href

        route =
            flags.href
                |> Routing.router
                |> Result.toMaybe

        anon =
            flags.key == Nothing

        boot =
            url
                |> Maybe.andThen Routing.parseBoot

        signupCmd =
            boot
                |> Maybe.andThen
                    (\s ->
                        case s of
                            Types.BootSignup str ->
                                Just str

                            _ ->
                                Nothing
                    )
                |> unwrap Cmd.none
                    (Data.check
                        >> Task.attempt Types.CheckCb
                    )
    in
    ( { emptyModel
        | screen = flags.screen
        , isMobile = flags.isMobile
        , charge = flags.charge
        , month = numberToMonth flags.month |> Maybe.withDefault Time.Jan
        , year = flags.year
        , landscape = flags.screen.width > flags.screen.height
        , area = View.Misc.getArea flags.screen
        , swActive = flags.swActive
        , funnel =
            boot
                |> unwrap Types.Hello
                    (\atn ->
                        case atn of
                            Types.BootSignup str ->
                                Types.Signup str

                            Types.BootPaymentFail ->
                                Types.PayErr

                            Types.BootPaymentSuccess ->
                                Types.PayOk
                    )
        , current =
            if anon then
                Nothing

            else
                route
                    |> Maybe.andThen
                        (\r ->
                            case r of
                                Types.RouteDay d ->
                                    Just d

                                _ ->
                                    Nothing
                        )
        , view =
            if anon then
                emptyModel.view

            else
                route
                    |> unwrap emptyModel.view
                        (\r ->
                            case r of
                                Types.RouteHome ->
                                    Types.ViewCalendar

                                Types.RouteTags ->
                                    Types.ViewTags

                                Types.RouteTag ->
                                    Types.ViewTags

                                Types.RouteSettings ->
                                    Types.ViewSettings

                                Types.RouteCalendar ->
                                    Types.ViewCalendar

                                Types.RouteDay _ ->
                                    Types.ViewCalendar

                                Types.RouteDayDetail _ ->
                                    Types.ViewCalendar

                                Types.RouteDayTags _ ->
                                    Types.ViewCalendar
                        )
      }
    , [ Helpers.today
            |> Task.perform Types.TodaySet
      , flags.key
            |> unwrap signupCmd
                (JD.decodeString JD.value
                    >> Result.toMaybe
                    >> unwrap Cmd.none
                        (\key ->
                            Data.refresh
                                |> Task.map
                                    (Maybe.map
                                        (\token ->
                                            { token = token, key = key }
                                        )
                                    )
                                |> Task.attempt (Types.InitCb route)
                        )
                )
      ]
        |> Cmd.batch
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onVisibilityChange Types.VisibilityChange
        , Ports.paymentFail (always Types.PaymentFail)
        , Ports.onUrlChange
            (Routing.router
                >> Types.UrlChange
            )
        , Browser.Events.onResize Screen
            |> Sub.map Types.Resize
        ]


emptyModel : Model
emptyModel =
    { errors = []
    , isMobile = False
    , posts = Day.newDayDict
    , tags = UD.empty
    , view = Types.ViewHome
    , postCreateTags = []
    , postEditorBody = ""
    , postBeingEdited = False
    , postView = False
    , tagView = False
    , auth = Nothing
    , tagCreateName = ""
    , tagUpdate = ""
    , tagBeingEdited = Nothing
    , flash = Nothing
    , loginForm =
        { password = ""
        , email = ""
        , passwordConfirm = ""
        , passwordVisible = False
        }
    , searchString = ""
    , screen = { height = 0, width = 0 }
    , month = Time.Jan
    , year = 2020
    , current = Nothing
    , funnel = Types.Hello
    , tag = Nothing
    , def = Nothing
    , magic = Nothing
    , inProgress =
        { logout = False
        , login = False
        , post = False
        , tag = False
        , postDelete = False
        , buy = False
        , tags = []
        , postTags = []
        }
    , thanks = False
    , swActive = False
    , tagsView = Types.TagsView
    , tagsSort = Types.SortName
    , tagsSortReverse = False
    , postSortReverse = False
    , weekStart = Time.Mon
    , today = 0 |> Time.millisToPosix |> Calendar.fromPosix
    , area = 0
    , landscape = False
    , charge = 0
    }
