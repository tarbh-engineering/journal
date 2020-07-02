module Main exposing (main)

import Browser
import Browser.Events
import Data
import Day
import Derberos.Date.Utils exposing (numberToMonth)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Helpers.UuidDict as UD
import Json.Decode as JD
import Maybe.Extra exposing (orElse, unwrap)
import Ports
import Routing
import Task
import Time exposing (Month(..))
import Types exposing (Flags, Model, Msg(..), Route(..), Screen, Sort(..), Status(..), View(..))
import Update exposing (update)
import Url
import Url.Parser exposing ((</>), parse, s)
import View exposing (view)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onVisibilityChange VisibilityChange
                    , Ports.online SetOnline
                    , Ports.onUrlChange
                        (Url.fromString
                            >> Maybe.andThen Routing.router
                            >> Types.UrlChange
                        )
                    , Browser.Events.onResize Screen
                        |> Sub.map Resize
                    ]
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        url =
            flags.href
                |> Url.fromString

        route =
            url
                |> Maybe.andThen Routing.router

        anon =
            flags.auth == Nothing

        model =
            { emptyModel
                | screen = flags.screen
                , isMobile = flags.isMobile
                , current =
                    if anon then
                        Nothing

                    else
                        route
                            |> Maybe.andThen
                                (\r ->
                                    case r of
                                        RouteDay d ->
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
                                        RouteToday ->
                                            ViewCalendar

                                        RouteHome ->
                                            ViewCalendar

                                        RouteTags ->
                                            ViewTags

                                        RouteStats ->
                                            ViewStats

                                        RouteSettings ->
                                            ViewSettings

                                        RouteCalendar ->
                                            ViewCalendar

                                        RouteDay _ ->
                                            ViewCalendar
                                )
                , month = numberToMonth flags.month |> Maybe.withDefault Time.Jan
                , year = flags.year
            }
    in
    flags.auth
        |> Maybe.andThen
            (JD.decodeString JD.value
                >> Result.toMaybe
            )
        |> unwrap
            (url
                |> Maybe.andThen
                    (\url_ ->
                        url_
                            |> Url.Parser.parse
                                (s "signup"
                                    </> Url.Parser.string
                                    </> Url.Parser.string
                                    |> Url.Parser.map Tuple.pair
                                )
                            |> Maybe.map
                                (\( iv, ciph ) ->
                                    ( { model
                                        | view = ViewMagic
                                        , mg = ( iv, ciph )
                                      }
                                    , Data.check iv ciph
                                        |> Task.attempt CheckCb
                                    )
                                )
                            |> orElse
                                (url_
                                    |> Url.Parser.parse (s "payment-success")
                                    |> Maybe.map
                                        (\_ ->
                                            ( { model | view = ViewSuccess }
                                            , Cmd.none
                                            )
                                        )
                                )
                    )
                |> Maybe.withDefault
                    ( model
                    , Cmd.none
                    )
            )
            (\key ->
                ( model
                , Data.refresh
                    |> Task.map
                        (Maybe.map
                            (\token ->
                                { token = token, key = key }
                            )
                        )
                    |> Task.attempt (InitCb route)
                )
            )


emptyModel : Model
emptyModel =
    { errors = []
    , isMobile = False
    , posts = Day.newDayDict
    , tags = UD.empty
    , view = ViewHome
    , postCreateTags = []
    , postEditorBody = ""
    , postBeingEdited = False
    , postView = False
    , auth = Nothing
    , tagCreateName = ""
    , tagUpdate = ""
    , tagBeingEdited = Nothing
    , flash = Nothing
    , loginForm =
        { password = ""
        , email = ""
        , passwordVisible = False
        }
    , tagSort = Alpha False
    , online = True
    , searchString = ""
    , selectedResult = Nothing
    , screen = { height = 0, width = 0 }
    , month = Time.Jan
    , year = 2020
    , current = Nothing
    , funnel = Types.Hello
    , tag = Nothing
    , def = Nothing
    , magic = Nothing
    , mg = ( "", "" )
    , inProgress =
        { logout = False
        , login = False
        , post = False
        , postDelete = False
        }
    , thanks = False
    }
