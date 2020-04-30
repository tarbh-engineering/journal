module Main exposing (main)

import Api.Scalar exposing (Uuid(..))
import Browser
import Browser.Events
import Data exposing (fetchTags, range)
import Date
import Day
import Derberos.Date.Utils exposing (numberToMonth)
import Dict
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Helpers.UuidDict as UD
import Lorem
import Maybe.Extra exposing (unwrap)
import Ports
import Random
import Random.List exposing (choose)
import Routing
import Set
import Task
import Time exposing (Month(..))
import Types exposing (Flags, Model, Msg(..), Post, PostView(..), Route(..), Screen, ServiceWorkerRequest(..), Sort(..), Status(..), Tag, View(..))
import Update exposing (update)
import Url
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
                            >> unwrap Types.NotFound
                                Routing.router
                            >> Types.UrlChange
                        )
                    , Browser.Events.onResize Screen
                        |> Sub.map Resize
                    ]
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        now =
            Task.map2
                Date.fromPosix
                Time.here
                Time.now

        startModel =
            { emptyModel
                | screen = flags.screen
                , month = numberToMonth flags.month |> Maybe.withDefault Time.Jan
                , year = flags.year
            }
    in
    flags.auth
        |> unwrap
            ( startModel
            , Routing.goTo RouteHome
            )
            (\auth ->
                ( { startModel
                    | auth = Just auth
                    , view = ViewHome
                    , online = flags.online
                    , force = False
                  }
                , Cmd.batch
                    [ now
                        |> Task.andThen
                            (\t ->
                                let
                                    start =
                                        Date.floor Date.Month t
                                in
                                range
                                    start
                                    (start
                                        |> Date.add Date.Months 1
                                    )
                                    auth
                            )
                        |> Task.attempt PostsCb
                    , fetchTags auth
                        |> Task.attempt TagsCb
                    , flags.href
                        |> Url.fromString
                        |> unwrap Types.NotFound Routing.router
                        |> Task.succeed
                        |> Task.perform UrlChange
                    ]
                )
            )


emptyModel : Model
emptyModel =
    { errors = []
    , posts = Day.newDayDict
    , tags = UD.empty
    , view = ViewLogin
    , postCreateTags = []
    , postSaveInProgress = False
    , postEditorBody = ""
    , postBeingEdited = False
    , postView = PostView
    , auth = Nothing
    , tagCreateName = ""
    , tagsBeingEdited = UD.empty
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
    , force = True
    , funnel = Types.Hello
    , tag = Nothing
    }
