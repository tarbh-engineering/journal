module Update exposing (update)

import Browser
import Browser.Dom
import Browser.Navigation as Navigation
import Calendar exposing (Date)
import Crypto
import Data
import DateTime
import Day
import Derberos.Date.Utils exposing (getNextMonth, getPrevMonth)
import Dict
import File.Download
import Graphql.Http
import Helpers
import Helpers.Parse
import Helpers.UuidDict as UD
import Json.Decode as JD
import Json.Encode as JE
import Lorem
import Maybe.Extra exposing (isNothing, unwrap)
import Ports
import Process
import Random
import Result.Extra exposing (unpack)
import Routing exposing (goTo)
import Task exposing (Task)
import Time
import Types exposing (Auth, GqlResult, GqlTask, Model, Msg(..), Post, Route(..))
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PaymentFail ->
            ( { model
                | inProgress =
                    model.inProgress
                        |> (\p ->
                                { p
                                    | annualPlan = False
                                    , monthlyPlan = False
                                }
                           )
              }
            , Cmd.none
            )

        TodaySet d ->
            ( { model | today = d }
            , Cmd.none
            )

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

        Resize screen ->
            ( { model | screen = screen }, Cmd.none )

        FakeData ->
            ( { model
                | tags =
                    Lorem.words 20
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
                                , created =
                                    1599320750750
                                        |> Time.millisToPosix
                                        |> DateTime.fromPosix
                                        |> (\d ->
                                                d
                                                    |> DateTime.setDay (modBy 28 n)
                                                    |> Maybe.andThen (DateTime.setHours (modBy 24 n))
                                                    |> Maybe.withDefault d
                                           )
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
                        , Ports.clearState ()
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
                                , Ports.saveState auth.key
                                , route
                                    |> unwrap Cmd.none
                                        (\route_ ->
                                            case route_ of
                                                RouteToday ->
                                                    Helpers.today
                                                        |> Task.perform
                                                            (RouteDay >> NavigateTo)

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

        PostCreateSubmit ->
            model.current
                |> unwrap ( model, Cmd.none )
                    (\d ->
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
                , postView =
                    model.current
                        |> Maybe.andThen (\d -> Day.get d model.posts)
                        |> Maybe.andThen .body
                        |> Maybe.Extra.isJust
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

        PostViewStart ->
            ( model
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
                        ( model
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
                        ( model
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
                            | inProgress =
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
                        ( model
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
                            | inProgress = model.inProgress |> (\p -> { p | post = False })
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
                            | inProgress = inProgress
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
                            | inProgress = inProgress
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
                            | inProgress =
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
                            , tagsView = Types.TagsView
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
                    |> Task.map2
                        (\t uuid ->
                            { id = uuid
                            , name = model.tagCreateName
                            , count = 0
                            , created = t
                            }
                                |> Ok
                        )
                        Helpers.now
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
                            , inProgress =
                                model.inProgress
                                    |> (\p -> { p | login = False })
                          }
                        , logGqlError "LoginCb" err
                        )
                    )
                    (\auth ->
                        ( { model
                            | auth = Just auth
                            , view = Types.ViewCalendar
                            , inProgress =
                                model.inProgress
                                    |> (\p -> { p | login = False })
                          }
                        , Cmd.batch
                            [ fetchCurrent auth
                                |> Task.attempt PostsCb
                            , Data.tags auth
                                |> Task.attempt TagsCb
                            , Ports.saveState auth.key
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
                            | inProgress = inProgress
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
                    |> Task.map (always <| Err <| Data.makeGqlCode "no-purchase")
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
            let
                inProgress =
                    model.inProgress |> (\p -> { p | login = False })
            in
            res
                |> unpack
                    (\err ->
                        if codeCheck "no-user" err then
                            ( { model
                                | funnel = Types.CheckEmail
                                , inProgress = inProgress
                              }
                            , Cmd.none
                            )

                        else if codeCheck "no-purchase" err then
                            ( { model
                                | funnel = Types.JoinUs
                                , inProgress = inProgress
                              }
                            , Cmd.none
                            )

                        else
                            ( { model
                                | errors = parseErrors err
                                , inProgress = inProgress
                              }
                            , logGqlError "NonceCb" err
                            )
                    )
                    (\nonce ->
                        ( { model
                            | funnel = Types.WelcomeBack nonce
                            , inProgress = inProgress
                          }
                        , Cmd.none
                        )
                    )

        LoginSubmit nonce ->
            let
                email =
                    String.trim model.loginForm.email
            in
            if String.isEmpty email || String.isEmpty model.loginForm.password then
                ( { model | errors = [ "empty field(s)" ] }
                , Cmd.none
                )

            else
                ( { model
                    | errors = []
                    , inProgress =
                        model.inProgress
                            |> (\p -> { p | login = True })
                  }
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
                [ Ports.clearState ()
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
            , wait
                |> Task.perform (always PaymentFail)
              --, Ports.buy
              --{ email = model.loginForm.email
              --, annual = annual
              --}
            )

        Boot { key, href, swActive } ->
            let
                url =
                    Url.fromString href

                route =
                    href
                        |> Routing.router
                        |> Result.toMaybe

                anon =
                    key == Nothing

                --url_
                --|> Url.Parser.parse
                --(s "signup"
                --</> Url.Parser.string
                --</> Url.Parser.string
                --|> Url.Parser.map Tuple.pair
                --)
                --|> Maybe.map
                --(\( iv, ciph ) ->
                --( { model
                --| view = ViewSignup
                --, mg = ( iv, ciph )
                --}
                --, Data.check iv ciph
                --|> Task.attempt CheckCb
                --)
                --)
                --|> orElse
                --(url_
                --|> Url.Parser.parse (s "payment-success")
                --|> Maybe.map
                --(\_ ->
                --( { model | view = ViewSuccess }
                --, Cmd.none
                --)
                --)
                --)
                signup =
                    url
                        |> Maybe.andThen Routing.parseSignup

                signupCmd =
                    signup
                        |> unwrap Cmd.none
                            (Data.check
                                >> Task.attempt CheckCb
                            )
            in
            ( { model
                | status = Types.Ready
                , swActive = swActive
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
                    if Maybe.Extra.isJust signup then
                        signup
                            |> Maybe.withDefault ""
                            |> Types.ViewSignup

                    else if anon then
                        model.view

                    else
                        route
                            |> unwrap model.view
                                (\r ->
                                    case r of
                                        RouteToday ->
                                            Types.ViewCalendar

                                        RouteHome ->
                                            Types.ViewCalendar

                                        RouteTags ->
                                            Types.ViewTags

                                        RouteTag ->
                                            Types.ViewTags

                                        RouteSettings ->
                                            Types.ViewSettings

                                        RouteCalendar ->
                                            Types.ViewCalendar

                                        RouteDay _ ->
                                            Types.ViewCalendar

                                        RouteDayDetail _ ->
                                            Types.ViewCalendar
                                )
              }
            , if Maybe.Extra.isJust signup then
                signupCmd

              else
                key
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

        SignupSubmit ciph ->
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
                                        Data.signup keys.serverKey nonce ciph
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
                        ( model
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
                        ( model
                        , logGqlError "TagUpdateCb" err
                        )
                    )
                    (unwrap
                        ( { model | auth = Nothing }
                        , Cmd.batch
                            [ Ports.clearState ()
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

        TagsSortSet sort ->
            ( { model
                | tagsSort = sort
                , tagsSortReverse =
                    if sort == model.tagsSort then
                        not model.tagsSortReverse

                    else
                        False
              }
            , Cmd.none
            )

        TagsViewSet t ->
            ( { model
                | tagsView = t
              }
            , if t == Types.TagsCreate then
                focusOnEditor

              else
                Cmd.none
            )

        WeekdaySet wd ->
            ( { model
                | weekStart = wd
              }
            , Cmd.none
            )

        PostSortToggle ->
            ( { model
                | postSortReverse = not model.postSortReverse
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
                    if model.view == Types.ViewHome then
                        Types.ViewCalendar

                    else
                        Types.ViewHome
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
                |> unpack
                    (\err ->
                        ( model_
                        , Ports.log err
                        )
                    )
                    (handleRoute model_)

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


handleRoute : Model -> Route -> ( Model, Cmd Msg )
handleRoute model route =
    let
        anon =
            isNothing model.auth
    in
    case route of
        RouteToday ->
            ( model
            , Helpers.today
                |> Task.perform (RouteDay >> NavigateTo)
            )

        RouteHome ->
            ( { model
                | view = Types.ViewHome
              }
            , Cmd.none
            )

        RouteTag ->
            ( { model | view = Types.ViewTags }, Cmd.none )

        RouteTags ->
            ( { model
                | view = Types.ViewTags
                , tag =
                    if isWide model.screen then
                        model.tag

                    else
                        Nothing
                , tags =
                    if anon then
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

                    else
                        model.tags
              }
            , model.auth
                |> unwrap Cmd.none
                    (trip
                        Data.tags
                        TagsCb
                    )
            )

        RouteSettings ->
            ( { model
                | view = Types.ViewSettings
              }
            , Cmd.none
            )

        RouteCalendar ->
            ( { model
                | view = Types.ViewCalendar
                , postView = False
                , tagView = False
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
            model.auth
                |> unwrap
                    (model.posts
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
                                    , month = Calendar.getMonth d
                                    , year = Calendar.getYear d
                                    , view = Types.ViewCalendar
                                    , postView = False
                                  }
                                , if False then
                                    focusOnEditor

                                  else
                                    Cmd.none
                                )
                           )
                    )
                    (routeLiveDay model d)


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
                    , month = Calendar.getMonth d
                    , year = Calendar.getYear d
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
                    if codeCheck "invalid-jwt" err then
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
                        Calendar.setDay 1 t
                            |> Maybe.withDefault t
                in
                Data.range
                    start
                    (start
                        |> Calendar.incrementMonth
                    )
                    auth
            )


codeCheck : String -> Graphql.Http.Error () -> Bool
codeCheck code err =
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
