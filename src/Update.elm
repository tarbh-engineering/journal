module Update exposing (update)

import Browser
import Browser.Dom
import Browser.Events exposing (Visibility(..))
import Browser.Navigation as Navigation
import Crypto
import Data
import Date exposing (Date)
import Day exposing (DayDict)
import Derberos.Date.Utils exposing (getNextMonth, getPrevMonth)
import Dict
import File.Download
import Graphql.Http exposing (HttpError(..), RawError(..))
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Helpers
import Helpers.Parse
import Helpers.UuidDict as UD
import Json.Decode as JD
import Json.Encode as JE
import Maybe.Extra exposing (isNothing, unwrap)
import Ports
import Process
import Random
import Result.Extra exposing (unpack)
import Routing exposing (goTo)
import Task exposing (Task)
import Time exposing (Month(..))
import Types exposing (Auth, GqlResult, GqlTask, Model, Msg(..), Post, Route(..), Sort(..), Status(..), View(..))
import Url
import Uuid
import Validate exposing (isValidEmail)
import View.Misc exposing (isWide)


wait : Task Never ()
wait =
    Process.sleep 300


focusOnEditor : Cmd Msg
focusOnEditor =
    Browser.Dom.focus "editor"
        |> Task.attempt FocusCb


clearLoading : DayDict (Status a) -> DayDict (Status a)
clearLoading =
    Day.map
        (\_ s ->
            case s of
                Loading ms ->
                    ms
                        |> unwrap Missing Found

                _ ->
                    s
        )
        >> Day.filter
            (\s ->
                s /= Missing
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GoToToday ->
            ( model
            , Helpers.today
                |> Task.perform (RouteDay >> NavigateTo)
            )

        ExportPosts ->
            ( model
            , model.posts
                |> Day.values
                |> JE.list
                    (\p ->
                        [ ( "day"
                          , p.date
                                |> Day.toString
                                |> JE.string
                          )
                        , ( "body"
                          , p.body
                                |> Maybe.withDefault ""
                                |> JE.string
                          )
                        , ( "id"
                          , p.id
                                |> Uuid.toString
                                |> JE.string
                          )
                        , ( "tags"
                          , p.tags
                                |> JE.list
                                    (Uuid.toString
                                        >> JE.string
                                    )
                          )
                        ]
                            |> JE.object
                    )
                |> JE.encode 2
                |> File.Download.string "posts.json" "application/json"
            )

        NextMonth ->
            ( { model
                | year =
                    if model.month == Time.Dec then
                        model.year + 1

                    else
                        model.year
                , month = getNextMonth model.month
              }
            , Cmd.none
            )

        PrevMonth ->
            ( { model
                | year =
                    if model.month == Time.Jan then
                        model.year - 1

                    else
                        model.year
                , month = getPrevMonth model.month
              }
            , Cmd.none
            )

        SetSelectedResult id ->
            ( { model
                | selectedResult =
                    if model.selectedResult == Just id then
                        Nothing

                    else
                        Just id
              }
            , Cmd.none
            )

        SetOnline online ->
            ( { model | online = online }
            , Cmd.none
            )

        TagSortUpdate sort ->
            ( { model | tagSort = sort }, Cmd.none )

        Resize screen ->
            ( { model | screen = screen }, Cmd.none )

        FakeData ->
            ( { model
                | tags =
                    [ "nulla"
                    , "labore"
                    , "voluptate"
                    , "qui"
                    , "eiusmod"
                    , "exercitation"
                    , "veniam"
                    , "ut"
                    , "et"
                    , "consectetur"
                    , "veniam"
                    , "aliqua"
                    , "elit"
                    , "do"
                    , "nostrud"
                    , "aliquip"
                    , "cupidatat"
                    , "nostrud"
                    , "enim"
                    , "ipsum"
                    , "aliquip"
                    , "culpa"
                    , "ut"
                    , "eu"
                    , "aute"
                    , "eiusmod"
                    , "elit"
                    , "irure"
                    , "fugiat"
                    , "culpa"
                    ]
                        |> List.indexedMap
                            (\n str ->
                                let
                                    id =
                                        n
                                            |> Random.initialSeed
                                            |> Random.step Uuid.uuidGenerator
                                            |> Tuple.first
                                in
                                { id = id
                                , name = str
                                , count = 0
                                }
                            )
                        |> UD.fromList
              }
            , Cmd.none
            )

        InitCb route res ->
            res
                |> unpack
                    (\err ->
                        ( model
                        , logGqlError "InitCb" err
                        )
                    )
                    (unwrap
                        ( model
                        , Ports.clearAuth ()
                        )
                        (\auth ->
                            ( { model
                                | auth = Just auth
                              }
                            , Cmd.batch
                                [ fetchCurrent auth
                                    |> Task.attempt PostsCb
                                , Data.tags auth
                                    |> Task.attempt TagsCb
                                , Ports.saveAuth auth.key
                                , route
                                    |> unwrap Cmd.none
                                        (\route_ ->
                                            case route_ of
                                                RouteToday ->
                                                    Task.map2
                                                        Date.fromPosix
                                                        Time.here
                                                        Time.now
                                                        |> Task.perform (RouteDay >> NavigateTo)

                                                RouteHome ->
                                                    Cmd.none

                                                RouteTags ->
                                                    Cmd.none

                                                RouteTag ->
                                                    Cmd.none

                                                RouteSettings ->
                                                    Cmd.none

                                                RouteCalendar ->
                                                    Cmd.none

                                                RouteStats ->
                                                    Cmd.none

                                                RouteDayDetail _ ->
                                                    Cmd.none

                                                RouteDay d ->
                                                    Data.fetchDay d auth
                                                        |> Task.attempt (PostCb d)
                                        )
                                ]
                            )
                        )
                    )

        PostViewCancel ->
            ( { model | postView = False, tagView = False }
            , Cmd.none
            )

        FocusCb res ->
            res
                |> unpack
                    (\(Browser.Dom.NotFound id) ->
                        ( model
                        , Ports.log <| "id not found: \"" ++ id ++ "\""
                        )
                    )
                    (\_ ->
                        ( model
                        , Cmd.none
                        )
                    )

        PostCreateSubmit d ->
            if String.isEmpty model.postEditorBody then
                ( { model | postView = False, postBeingEdited = False }, Cmd.none )

            else if model.auth == Nothing then
                ( { model
                    | inProgress = model.inProgress |> (\p -> { p | post = True })
                  }
                , wait
                    |> Task.andThen (always Time.now)
                    |> Task.map
                        (Time.posixToMillis
                            >> Random.initialSeed
                            >> Random.step Uuid.uuidGenerator
                            >> Tuple.first
                            >> (\uuid ->
                                    { id = uuid
                                    , body = Just model.postEditorBody
                                    , tags = []
                                    , date = d
                                    }
                                        |> Ok
                               )
                        )
                    |> Task.perform PostMutateCb
                )

            else
                ( { model | inProgress = model.inProgress |> (\p -> { p | post = True }) }
                , model.auth
                    |> unwrap Cmd.none
                        (trip
                            (Data.postCreate model.postEditorBody
                                model.postCreateTags
                                d
                            )
                            PostMutateCb
                        )
                )

        PostDelete id date ->
            ( { model
                | inProgress =
                    model.inProgress
                        |> (\p -> { p | postDelete = True })
              }
            , if model.auth == Nothing then
                wait
                    |> Task.map (always <| Ok date)
                    |> Task.perform PostDeleteCb

              else
                model.auth
                    |> unwrap Cmd.none
                        (trip (Data.postDelete id)
                            PostDeleteCb
                        )
            )

        PostUpdateCancel ->
            ( { model
                | postBeingEdited = False
              }
            , Cmd.none
            )

        PostViewTagStart ->
            ( { model
                | tagView = True
              }
            , if isWide model.screen then
                Cmd.none

              else
                model.current
                    |> unwrap
                        Cmd.none
                        (RouteDayDetail >> goTo)
            )

        PostViewToggle ->
            ( { model
                | tagView = True
              }
            , if isWide model.screen then
                Cmd.none

              else
                model.current
                    |> unwrap
                        Cmd.none
                        (RouteDayDetail >> goTo)
            )

        TagViewToggle ->
            ( { model
                | tagView = not model.tagView
              }
            , Cmd.none
            )

        PostUpdateStart ->
            ( { model
                | postEditorBody =
                    model.current
                        |> Maybe.andThen (\d -> Day.get d model.posts)
                        |> Maybe.andThen .body
                        |> Maybe.withDefault ""
                , postBeingEdited = True
                , tagView = False
              }
            , if isWide model.screen then
                focusOnEditor

              else
                model.current
                    |> unwrap
                        Cmd.none
                        (RouteDayDetail >> goTo)
            )

        PostClear post ->
            ( { model
                | inProgress =
                    model.inProgress
                        |> (\p ->
                                { p | post = True }
                           )
              }
            , wait
                |> Task.map
                    ({ post
                        | body = Nothing
                     }
                        |> Just
                        |> Ok
                        |> always
                    )
                |> Task.perform (PostCb post.date)
            )

        PostUpdateSubmit id ->
            ( { model | inProgress = model.inProgress |> (\p -> { p | post = True }) }
            , model.auth
                |> unwrap
                    (model.current
                        |> Maybe.andThen (\d -> Day.get d model.posts)
                        |> unwrap Cmd.none
                            (\p ->
                                wait
                                    |> Task.map
                                        (always <|
                                            Ok
                                                { p | body = Just model.postEditorBody }
                                        )
                                    |> Task.perform PostMutateCb
                            )
                    )
                    (trip (Data.postUpdateBody id model.postEditorBody) PostMutateCb)
            )

        PostsCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model
                            | online = not <| isNetworkError err
                          }
                        , logGqlError "PostsCb" err
                        )
                    )
                    (\posts ->
                        ( { model
                            | posts =
                                posts
                                    |> List.foldr
                                        (\v ->
                                            Day.insert v.date v
                                        )
                                        model.posts
                          }
                        , Cmd.none
                        )
                    )

        TagsCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model | online = not <| isNetworkError err }
                        , logGqlError "TagsCb" err
                        )
                    )
                    (\tags ->
                        ( { model
                            | tags =
                                tags
                                    |> UD.fromList
                          }
                        , Cmd.none
                        )
                    )

        PostDeleteCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model
                            | online = not <| isNetworkError err
                            , inProgress =
                                model.inProgress
                                    |> (\p -> { p | postDelete = False })
                            , postBeingEdited = False
                          }
                        , logGqlError "PostDeleteCb" err
                        )
                    )
                    (\date ->
                        ( { model
                            | posts =
                                model.posts
                                    |> Day.remove date
                            , postEditorBody = ""
                            , inProgress = model.inProgress |> (\p -> { p | postDelete = False })
                            , current = Nothing
                            , postView = False
                            , postBeingEdited = False
                          }
                        , Cmd.none
                        )
                    )

        TagDeleteCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model | online = not <| isNetworkError err }
                        , logGqlError "TagDeleteCb" err
                        )
                    )
                    (\id ->
                        ( { model
                            | tags =
                                UD.remove
                                    id
                                    model.tags
                          }
                        , Cmd.none
                        )
                    )

        PostMutateCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model
                            | online = not <| isNetworkError err
                            , inProgress = model.inProgress |> (\p -> { p | post = False })
                            , postBeingEdited = False
                          }
                        , logGqlError "PostMutateCb" err
                        )
                    )
                    (\post ->
                        ( { model
                            | posts =
                                model.posts
                                    |> Day.insert
                                        post.date
                                        post
                            , postBeingEdited = False
                            , inProgress = model.inProgress |> (\p -> { p | post = False })
                            , online = True
                            , postView = model.isMobile
                          }
                        , Cmd.none
                        )
                    )

        PostTagCb id res ->
            let
                inProgress =
                    model.inProgress
                        |> (\p ->
                                { p
                                    | tags =
                                        p.tags
                                            |> List.filter ((/=) id)
                                }
                           )
            in
            res
                |> unpack
                    (\err ->
                        ( { model
                            | online = not <| isNetworkError err
                            , inProgress = inProgress
                          }
                        , logGqlError "PostTagCb" err
                        )
                    )
                    (\post ->
                        ( { model
                            | posts =
                                model.posts
                                    |> Day.insert
                                        post.date
                                        post
                            , inProgress = inProgress
                          }
                        , Cmd.none
                        )
                    )

        PostCb day res ->
            let
                inProgress =
                    model.inProgress |> (\p -> { p | post = False })
            in
            res
                |> unpack
                    (\err ->
                        ( { model
                            | online = not <| isNetworkError err
                            , inProgress = inProgress
                            , postBeingEdited = False
                          }
                        , logGqlError "PostCb" err
                        )
                    )
                    (\data ->
                        case data of
                            Just post ->
                                ( { model
                                    | posts =
                                        model.posts
                                            |> Day.insert
                                                post.date
                                                post
                                    , postBeingEdited = False
                                    , inProgress = inProgress
                                    , postView =
                                        if isNothing post.body then
                                            False

                                        else
                                            model.postView
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                ( { model
                                    | posts =
                                        model.posts
                                            |> Day.remove day
                                    , postBeingEdited = False
                                    , inProgress = inProgress
                                  }
                                , Cmd.none
                                )
                    )

        TagCreateCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model
                            | online = not <| isNetworkError err
                            , inProgress =
                                model.inProgress
                                    |> (\p -> { p | tag = False })
                          }
                        , logGqlError "TagCreateCb" err
                        )
                    )
                    (\tag ->
                        ( { model
                            | tags =
                                UD.insert tag.id tag model.tags
                            , tagCreateName = ""
                            , tagCreate = False
                            , inProgress =
                                model.inProgress
                                    |> (\p -> { p | tag = False })
                          }
                        , Cmd.none
                        )
                    )

        LogoutCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model
                            | inProgress = model.inProgress |> (\p -> { p | logout = False })
                          }
                        , logGqlError "LogoutCb" err
                        )
                    )
                    (\_ ->
                        ( { model
                            | posts = Day.newDayDict
                            , auth = Nothing
                            , tags = UD.empty
                            , current = Nothing
                            , inProgress =
                                model.inProgress
                                    |> (\p ->
                                            { p | logout = False }
                                       )
                            , loginForm =
                                { email = ""
                                , password = ""
                                , passwordVisible = False
                                }
                            , funnel = Types.Hello
                          }
                        , goTo RouteHome
                        )
                    )

        TagCreateSubmit ->
            if String.isEmpty model.tagCreateName then
                ( model
                , Cmd.none
                )

            else if model.auth == Nothing then
                ( { model
                    | inProgress =
                        model.inProgress
                            |> (\p -> { p | tag = True })
                  }
                , wait
                    |> Task.andThen
                        (randomTask Uuid.uuidGenerator
                            |> always
                        )
                    |> Task.map
                        (\uuid ->
                            { id = uuid
                            , name = model.tagCreateName
                            , count = 0
                            }
                                |> Ok
                        )
                    |> Task.perform TagCreateCb
                )

            else
                ( { model
                    | inProgress =
                        model.inProgress
                            |> (\p -> { p | tag = True })
                  }
                , model.auth
                    |> unwrap Cmd.none
                        (trip (Data.tagCreate model.tagCreateName)
                            TagCreateCb
                        )
                )

        BodyUpdate str ->
            ( { model | postEditorBody = str }, Cmd.none )

        LoginCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model
                            | errors = parseErrors err
                            , online = not <| isNetworkError err
                          }
                        , logGqlError "LoginCb" err
                        )
                    )
                    (\auth ->
                        ( { model
                            | auth = Just auth
                            , view = ViewCalendar
                          }
                        , Cmd.batch
                            [ fetchCurrent auth
                                |> Task.attempt PostsCb
                            , Data.tags auth
                                |> Task.attempt TagsCb
                            , Ports.saveAuth auth.key
                            ]
                        )
                    )

        TagCreateNameUpdate str ->
            ( { model | tagCreateName = str }, Cmd.none )

        SetDef d ->
            ( { model
                | def =
                    if Just d == model.def then
                        Nothing

                    else
                        Just d
              }
            , Cmd.none
            )

        PostCreateWithTag date tag ->
            ( { model
                | inProgress =
                    model.inProgress
                        |> (\p ->
                                { p
                                    | tags =
                                        tag.id :: p.tags
                                }
                           )
              }
            , model.auth
                |> unwrap
                    (wait
                        |> Task.andThen
                            (always <| randomTask Uuid.uuidGenerator)
                        |> Task.map
                            (\id ->
                                { tags = [ tag.id ]
                                , date = date
                                , body = Nothing
                                , id = id
                                }
                                    |> Ok
                            )
                        |> Task.perform (PostCreateWithTagCb tag.id)
                    )
                    (always Cmd.none)
            )

        PostCreateWithTagCb id res ->
            let
                inProgress =
                    model.inProgress
                        |> (\p ->
                                { p
                                    | tags =
                                        p.tags
                                            |> List.filter ((/=) id)
                                }
                           )
            in
            res
                |> unpack
                    (\err ->
                        ( { model
                            | online = not <| isNetworkError err
                            , inProgress = inProgress
                          }
                        , logGqlError "PostCreateTagCb" err
                        )
                    )
                    (\post ->
                        ( { model
                            | posts =
                                model.posts
                                    |> Day.insert
                                        post.date
                                        post
                            , inProgress = inProgress
                          }
                        , Cmd.none
                        )
                    )

        EmailSubmit ->
            let
                email =
                    String.trim model.loginForm.email
            in
            if String.isEmpty email then
                ( { model | errors = [ "empty field(s)" ] }
                , Cmd.none
                )

            else if isValidEmail email then
                ( { model
                    | errors = []
                    , inProgress =
                        model.inProgress
                            |> (\p -> { p | login = True })
                  }
                  --, Data.nonce email
                  --|> Task.attempt NonceCb
                , wait
                    |> Task.map (always <| Ok "")
                    |> Task.perform NonceCb
                )

            else
                ( { model | errors = [ "Invalid data" ] }
                , Cmd.none
                )

        EmailCb ->
            ( { model
                | inProgress =
                    model.inProgress
                        |> (\p -> { p | login = False })
                , thanks = True
                , loginForm =
                    model.loginForm
                        |> (\f -> { f | email = "" })
              }
            , Cmd.none
            )

        CheckCb res ->
            res
                |> unpack
                    (\err ->
                        ( model
                        , logGqlError "CheckCb" err
                        )
                    )
                    (\bool ->
                        ( { model | magic = Just bool }
                        , Cmd.none
                        )
                    )

        NonceCb res ->
            res
                |> unpack
                    (\err ->
                        if check "no-user" err then
                            ( { model
                                | funnel = Types.CheckEmail
                                , inProgress = model.inProgress |> (\p -> { p | login = False })
                              }
                            , Cmd.none
                            )

                        else if check "no-purchase" err then
                            ( { model
                                | funnel = Types.JoinUs
                                , inProgress = model.inProgress |> (\p -> { p | login = False })
                              }
                            , Cmd.none
                            )

                        else
                            ( { model
                                | errors = parseErrors err
                                , online = not <| isNetworkError err
                                , inProgress = model.inProgress |> (\p -> { p | login = False })
                              }
                            , logGqlError "NonceCb" err
                            )
                    )
                    (\_ ->
                        ( { model
                            --| funnel = Types.WelcomeBack nonce
                            | funnel = Types.JoinUs
                            , inProgress = model.inProgress |> (\p -> { p | login = False })
                          }
                        , Cmd.none
                        )
                    )

        LoginSubmit nonce ->
            let
                email =
                    String.trim model.loginForm.email
            in
            if String.isEmpty email then
                ( { model | errors = [ "empty field(s)" ] }
                , Cmd.none
                )

            else
                ( { model | errors = [] }
                , Crypto.keys model.loginForm.password nonce
                    |> Task.andThen
                        (\keys ->
                            Data.login email keys.serverKey
                                |> Task.map
                                    (\token ->
                                        { key = keys.encryptionKey
                                        , token = token
                                        }
                                    )
                        )
                    |> Task.attempt LoginCb
                )

        Change ->
            ( { model | funnel = Types.Hello }, Cmd.none )

        Logout ->
            ( { model
                | inProgress = model.inProgress |> (\p -> { p | logout = True })
              }
            , Cmd.batch
                [ Ports.clearAuth ()
                , Data.logout
                    |> Task.attempt LogoutCb
                ]
            )

        Buy annual ->
            ( { model
                | inProgress =
                    model.inProgress
                        |> (\p ->
                                { p
                                    | monthlyPlan = not annual
                                    , annualPlan = annual
                                }
                           )
              }
            , Ports.buy
                { email = model.loginForm.email
                , annual = annual
                }
            )

        Boot { key, href } ->
            let
                url =
                    Url.fromString href

                route =
                    url
                        |> Maybe.andThen Routing.router

                anon =
                    key == Nothing
            in
            ( { model
                | status = Types.Ready
                , current =
                    if anon then
                        model.current

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
                        model.view

                    else
                        route
                            |> unwrap model.view
                                (\r ->
                                    case r of
                                        RouteToday ->
                                            ViewCalendar

                                        RouteHome ->
                                            ViewCalendar

                                        RouteTags ->
                                            ViewTags

                                        RouteTag ->
                                            ViewTags

                                        RouteStats ->
                                            ViewStats

                                        RouteSettings ->
                                            ViewSettings

                                        RouteCalendar ->
                                            ViewCalendar

                                        RouteDay _ ->
                                            ViewCalendar

                                        RouteDayDetail d ->
                                            ViewCalendar
                                )
              }
            , key
                |> Maybe.andThen
                    (JD.decodeString JD.value
                        >> Result.toMaybe
                    )
                |> unwrap Cmd.none
                    (\key_ ->
                        Data.refresh
                            |> Task.map
                                (Maybe.map
                                    (\token ->
                                        { token = token, key = key_ }
                                    )
                                )
                            |> Task.attempt (InitCb route)
                    )
            )

        SignupSubmit ->
            let
                ( iv, ciph ) =
                    model.mg
            in
            if String.isEmpty model.loginForm.password then
                ( { model | errors = [ "empty field(s)" ] }
                , Cmd.none
                )

            else
                ( { model | errors = [] }
                , Crypto.nonce
                    |> Task.andThen
                        (\nonce ->
                            Crypto.keys model.loginForm.password nonce
                                |> Task.andThen
                                    (\keys ->
                                        Data.signup keys.serverKey nonce iv ciph
                                            |> Task.map
                                                (\token ->
                                                    { key = keys.encryptionKey
                                                    , token = token
                                                    }
                                                )
                                    )
                        )
                    |> Task.attempt LoginCb
                )

        TagDelete tag ->
            ( model
            , if model.auth == Nothing then
                tag.id
                    |> Task.succeed
                    |> Task.attempt TagDeleteCb

              else
                model.auth
                    |> unwrap Cmd.none
                        (trip (Data.tagDelete tag) TagDeleteCb)
            )

        TagUpdate value ->
            ( { model
                | tagUpdate = value
              }
            , Cmd.none
            )

        TagUpdateSet tag ->
            ( { model
                | tagBeingEdited = tag |> Maybe.map .id
                , tagUpdate = tag |> unwrap "" .name
              }
            , Cmd.none
            )

        TagUpdateSubmit t ->
            ( model
            , if model.auth == Nothing then
                { t | name = model.tagUpdate }
                    |> Task.succeed
                    |> Task.attempt TagUpdateCb

              else
                model.auth
                    |> unwrap Cmd.none
                        (trip (Data.tagUpdate t)
                            TagUpdateCb
                        )
            )

        TagUpdateCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model | online = not <| isNetworkError err }
                        , logGqlError "TagUpdateCb" err
                        )
                    )
                    (\tag ->
                        ( { model
                            | tags =
                                UD.insert
                                    tag.id
                                    tag
                                    model.tags
                            , tagBeingEdited = Nothing
                          }
                        , Cmd.none
                        )
                    )

        RefreshCb msg_ res ->
            res
                |> unpack
                    (\err ->
                        ( { model | online = not <| isNetworkError err }
                        , logGqlError "TagUpdateCb" err
                        )
                    )
                    (unwrap
                        ( { model | auth = Nothing }
                        , Cmd.batch
                            [ Ports.clearAuth ()
                            , goTo RouteHome
                            ]
                        )
                        (\tk ->
                            model.auth
                                |> unwrap
                                    ( model, Cmd.none )
                                    (\auth ->
                                        ( { model
                                            | auth = Just { auth | token = tk }
                                          }
                                        , msg_ { auth | token = tk }
                                        )
                                    )
                        )
                    )

        PostTagToggle post tag ->
            ( { model
                | inProgress =
                    model.inProgress
                        |> (\p ->
                                { p
                                    | tags =
                                        tag.id :: p.tags
                                }
                           )
              }
            , model.auth
                |> unwrap
                    (wait
                        |> Task.map
                            ({ post
                                | tags =
                                    post.tags
                                        |> toggle tag.id
                             }
                                |> Ok
                                |> always
                            )
                        |> Task.perform (PostTagCb tag.id)
                    )
                    (if List.member tag.id post.tags then
                        trip
                            (Data.tagDetach tag.id
                                >> Task.map Just
                            )
                            (PostCb post.date)

                     else
                        trip
                            (Data.tagAttach post tag.id
                                >> Task.map Just
                            )
                            (PostCb post.date)
                    )
            )

        TagCreateToggle ->
            ( { model
                | tagCreate = not model.tagCreate
              }
            , Cmd.none
            )

        DropdownToggle ->
            ( { model
                | dropdown = not model.dropdown
              }
            , Cmd.none
            )

        FaqToggle ->
            ( { model
                | faq = not model.faq
              }
            , Cmd.none
            )

        Force ->
            ( { model
                | view =
                    if model.view == ViewHome then
                        ViewCalendar

                    else
                        ViewHome
                , def = Nothing
                , funnel = Types.Hello
                , current = Nothing
                , loginForm =
                    model.loginForm
                        |> (\f -> { f | email = "" })
              }
            , Cmd.none
            )

        LoginFormEmailUpdate str ->
            ( { model
                | loginForm =
                    model.loginForm
                        |> (\f -> { f | email = str })
              }
            , Cmd.none
            )

        LoginFormPasswordUpdate str ->
            ( { model
                | loginForm =
                    model.loginForm
                        |> (\f -> { f | password = str })
              }
            , Cmd.none
            )

        LoginFormPasswordVisibleToggle ->
            ( { model
                | loginForm =
                    model.loginForm
                        |> (\f -> { f | passwordVisible = not f.passwordVisible })
              }
            , Cmd.none
            )

        UrlRequest req ->
            ( model
            , (case req of
                Browser.Internal url ->
                    Url.toString url

                Browser.External str ->
                    str
              )
                |> Navigation.load
            )

        VisibilityChange _ ->
            ( model
            , Cmd.none
            )

        TagSelect id ->
            ( { model
                | tag = Just id
              }
              --id
              --|> unwrap Cmd.none
              --(\id_ ->
              --model.auth
              --|> unwrap Cmd.none
              --(trip (Data.fetchPostsByTag id_)
              --PostsCb
              --)
              --)
            , if isWide model.screen then
                Cmd.none

              else
                goTo RouteTag
            )

        TagDeselect ->
            ( { model
                | tag = Nothing
              }
            , if isWide model.screen then
                Cmd.none

              else
                goTo RouteTags
            )

        UrlChange r_ ->
            let
                model_ =
                    { model
                        | -- postBeingEdited = False
                          --, tagView = False
                          --, postView = False
                          dropdown = False
                    }
            in
            r_
                |> unwrap
                    ( model_
                    , Ports.log "Missing route"
                    )
                    (model.auth
                        |> unwrap
                            (routeDemo model_)
                            (routeLive model_)
                    )

        Bad mm ->
            ( model
            , Data.refresh
                |> Task.attempt (RefreshCb mm)
            )

        NavigateTo route ->
            ( model, goTo route )


randomTask : Random.Generator b -> Task a b
randomTask gen =
    Time.now
        |> Task.map
            (Time.posixToMillis
                >> Random.initialSeed
                >> Random.step gen
                >> Tuple.first
            )


routeDemo : Model -> Route -> ( Model, Cmd Msg )
routeDemo model route =
    case route of
        RouteToday ->
            ( model
            , Helpers.today
                |> Task.perform (RouteDay >> NavigateTo)
            )

        RouteHome ->
            ( { model
                | view = ViewHome
              }
            , Cmd.none
            )

        RouteTag ->
            ( { model | view = ViewTags }, Cmd.none )

        RouteTags ->
            ( { model
                | view = ViewTags
                , tag =
                    if isWide model.screen then
                        model.tag

                    else
                        Nothing
                , tags =
                    model.tags
                        |> UD.values
                        |> List.map
                            (\t ->
                                { t
                                    | count =
                                        model.posts
                                            |> Day.values
                                            |> List.filter
                                                (.tags >> List.member t.id)
                                            |> List.length
                                }
                            )
                        |> UD.fromList
              }
            , Cmd.none
            )

        RouteSettings ->
            ( { model
                | view = ViewSettings
              }
            , Cmd.none
            )

        RouteCalendar ->
            ( { model
                | view = ViewCalendar
                , postView = False
                , tagView = False
              }
            , Cmd.none
            )

        RouteStats ->
            ( { model
                | view = ViewStats
              }
            , Cmd.none
            )

        RouteDayDetail _ ->
            ( { model
                | postView = True
              }
            , if model.postBeingEdited && not model.tagView then
                focusOnEditor

              else
                Cmd.none
            )

        RouteDay d ->
            model.posts
                |> Day.get d
                |> (\data ->
                        let
                            shouldFocusOnEditor =
                                if data == Nothing then
                                    isWide model.screen

                                else
                                    isWide model.screen && model.postBeingEdited

                            editorText =
                                data |> Maybe.andThen .body |> Maybe.withDefault ""
                        in
                        ( { model
                            | postEditorBody = editorText
                            , postCreateTags = []
                            , postBeingEdited = shouldFocusOnEditor
                            , current = Just d
                            , month = Date.month d
                            , year = Date.year d
                            , view = ViewCalendar
                            , postView = False
                          }
                        , if False then
                            focusOnEditor

                          else
                            Cmd.none
                        )
                   )


routeLive : Model -> Auth -> Route -> ( Model, Cmd Msg )
routeLive model auth route =
    case route of
        RouteToday ->
            ( model
            , Helpers.today
                |> Task.perform (RouteDay >> NavigateTo)
            )

        RouteHome ->
            ( { model
                | view = ViewHome
              }
            , Cmd.none
            )

        RouteTag ->
            ( model, Cmd.none )

        RouteTags ->
            ( { model
                | view = ViewTags
              }
            , trip
                Data.tags
                TagsCb
                auth
            )

        RouteSettings ->
            ( { model
                | view = ViewSettings
              }
            , Cmd.none
            )

        RouteCalendar ->
            ( { model
                | view = ViewCalendar
              }
            , Cmd.none
            )

        RouteStats ->
            ( { model
                | view = ViewStats
              }
            , Cmd.none
            )

        RouteDayDetail d ->
            ( model, Cmd.none )

        RouteDay d ->
            routeLiveDay model d auth


routeLiveDay : Model -> Date -> Auth -> ( Model, Cmd Msg )
routeLiveDay model d auth =
    model.posts
        |> Day.get d
        |> (\data ->
                let
                    shouldFocusOnEditor =
                        if data == Nothing then
                            isWide model.screen

                        else
                            isWide model.screen && model.postBeingEdited

                    editorText =
                        data |> Maybe.andThen .body |> Maybe.withDefault ""
                in
                ( { model
                    | postEditorBody = editorText
                    , postCreateTags = []
                    , postBeingEdited = shouldFocusOnEditor
                    , inProgress = model.inProgress |> (\p -> { p | post = False })
                    , current = Just d
                    , month = Date.month d
                    , year = Date.year d
                    , postView = False
                  }
                , Cmd.batch
                    [ trip
                        (Data.fetchDay d)
                        (PostCb d)
                        auth
                    , if False then
                        focusOnEditor

                      else
                        Cmd.none
                    ]
                )
           )


isNetworkError : Graphql.Http.Error () -> Bool
isNetworkError err =
    case err of
        Graphql.Http.HttpError httpErr ->
            httpErr == Graphql.Http.NetworkError

        Graphql.Http.GraphqlError _ _ ->
            False


parseErrors : Graphql.Http.Error a -> List String
parseErrors err =
    case err of
        Graphql.Http.GraphqlError _ errs ->
            errs |> List.map .message

        _ ->
            [ "uh oh" ]


logGqlError : String -> Graphql.Http.Error a -> Cmd msg
logGqlError tag err =
    Ports.log (tag ++ ":\n" ++ Helpers.Parse.gqlError err)


toggle : a -> List a -> List a
toggle v ls =
    if List.member v ls then
        List.filter ((/=) v) ls

    else
        v :: ls


trip : (Auth -> GqlTask a) -> (GqlResult a -> Msg) -> Auth -> Cmd Msg
trip task msg =
    task
        >> Task.attempt
            (unpack
                (\err ->
                    if check "invalid-jwt" err then
                        Bad
                            (task
                                >> Task.attempt msg
                            )

                    else
                        err
                            |> Err
                            |> msg
                )
                (Ok >> msg)
            )


fetchCurrent : Auth -> GqlTask (List Post)
fetchCurrent auth =
    Helpers.today
        |> Task.andThen
            (\t ->
                let
                    start =
                        Date.floor Date.Month t
                in
                Data.range
                    start
                    (start
                        |> Date.add Date.Months 1
                    )
                    auth
            )


check : String -> Graphql.Http.Error () -> Bool
check code err =
    case err of
        Graphql.Http.GraphqlError _ es ->
            es
                |> List.concatMap (.details >> Dict.values)
                |> List.filterMap
                    (JD.decodeValue
                        (JD.field "code" JD.string)
                        >> Result.toMaybe
                    )
                |> List.any ((==) code)

        Graphql.Http.HttpError _ ->
            False
