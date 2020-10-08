module Update exposing (update)

import Browser.Dom
import Calendar exposing (Date)
import Crypto
import Data
import Day
import Derberos.Date.Utils exposing (getNextMonth, getPrevMonth)
import Dict
import Graphql.Http
import Helpers
import Helpers.Parse
import Helpers.UuidDict as UD
import Json.Decode as JD
import List.Extra
import Maybe.Extra exposing (isNothing, unwrap)
import Ports
import Process
import Random
import Result.Extra exposing (unpack)
import Routing exposing (goTo)
import Task exposing (Task)
import Time
import Types exposing (Auth, GqlResult, GqlTask, Model, Msg(..), Post, Route(..))
import Uuid
import Validate exposing (isValidEmail)


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
                                    | buy = False
                                }
                           )
              }
            , Cmd.none
            )

        TodaySet d ->
            ( { model | today = d }
            , Cmd.none
            )

        GoToToday md ->
            md
                |> unwrap
                    ( model
                    , Helpers.today
                        |> Task.perform (Just >> GoToToday)
                    )
                    (\d ->
                        ( { model | today = d }
                        , goTo <| RouteDay d
                        )
                    )

        ReadyStart md ->
            md
                |> unwrap
                    ( model
                    , Helpers.today
                        |> Task.perform (Just >> ReadyStart)
                    )
                    (\d ->
                        ( { model
                            | today = d
                            , current = Just d
                            , postBeingEdited = True
                            , postEditorBody =
                                Day.get d model.posts
                                    |> Maybe.andThen .body
                                    |> Maybe.withDefault ""
                          }
                        , goTo <| RouteDay d
                        )
                    )

        NextMonth ->
            let
                month =
                    getNextMonth model.month

                year =
                    if model.month == Time.Dec then
                        model.year + 1

                    else
                        model.year

                start_ =
                    Calendar.fromRawParts
                        { year = year
                        , month = month
                        , day = 1
                        }
            in
            ( { model
                | year = year
                , month = month
              }
            , start_
                |> unwrap Cmd.none
                    (\start ->
                        model.auth
                            |> unwrap Cmd.none
                                (trip
                                    (Data.range
                                        start
                                        (start
                                            |> Calendar.incrementMonth
                                        )
                                    )
                                    PostsCb
                                )
                    )
            )

        PrevMonth ->
            let
                month =
                    getPrevMonth model.month

                year =
                    if model.month == Time.Jan then
                        model.year - 1

                    else
                        model.year

                start_ =
                    Calendar.fromRawParts
                        { year = year
                        , month = month
                        , day = 1
                        }
            in
            ( { model
                | year = year
                , month = month
              }
            , start_
                |> unwrap Cmd.none
                    (\start ->
                        model.auth
                            |> unwrap Cmd.none
                                (trip
                                    (Data.range
                                        start
                                        (start
                                            |> Calendar.incrementMonth
                                        )
                                    )
                                    PostsCb
                                )
                    )
            )

        Resize screen ->
            ( { model
                | screen = screen
                , landscape =
                    if screen.width == model.screen.width then
                        model.landscape

                    else
                        screen.width > screen.height
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
                        , Cmd.batch
                            [ Ports.clearState ()
                            , goTo Types.RouteHome
                            ]
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

                                                RouteDayTags _ ->
                                                    Cmd.none

                                                RouteDay d ->
                                                    auth
                                                        |> trip (Data.fetchDay d)
                                                            (PostCb d)
                                        )
                                ]
                            )
                        )
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

        PostUpdateStart ->
            ( { model
                | postEditorBody =
                    model.current
                        |> Maybe.andThen (\d -> Day.get d model.posts)
                        |> Maybe.andThen .body
                        |> Maybe.withDefault ""
                , postBeingEdited = True
              }
            , focusOnEditor
            )

        PostBodySubmit ->
            model.current
                |> unwrap ( model, Cmd.none )
                    (\d ->
                        ( { model
                            | inProgress =
                                model.inProgress
                                    |> (\p -> { p | post = True })
                          }
                        , Day.get d model.posts
                            |> unwrap
                                (model.auth
                                    |> unwrap
                                        (wait
                                            |> Task.andThen
                                                (randomTask Uuid.uuidGenerator
                                                    |> always
                                                )
                                            |> Task.map
                                                (\uuid ->
                                                    { id = uuid
                                                    , body = Just model.postEditorBody
                                                    , tags = []
                                                    , date = d
                                                    }
                                                        |> Ok
                                                )
                                            |> Task.perform PostMutateCb
                                        )
                                        (trip
                                            (Data.postCreate model.postEditorBody
                                                model.postCreateTags
                                                d
                                            )
                                            PostMutateCb
                                        )
                                )
                                (\post ->
                                    model.auth
                                        |> unwrap
                                            (wait
                                                |> Task.map
                                                    ({ post
                                                        | body =
                                                            if model.postEditorBody == "" then
                                                                Nothing

                                                            else
                                                                Just model.postEditorBody
                                                     }
                                                        |> Ok
                                                        |> always
                                                    )
                                                |> Task.perform PostMutateCb
                                            )
                                            (trip
                                                (Data.postUpdateBody post.id model.postEditorBody)
                                                PostMutateCb
                                            )
                                )
                        )
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

        PostWithTagCb pair res ->
            let
                inProgress =
                    model.inProgress
                        |> (\p ->
                                { p
                                    | postTags =
                                        p.postTags
                                            |> List.filter ((/=) pair)
                                }
                           )
            in
            res
                |> unpack
                    (\err ->
                        ( { model
                            | inProgress = inProgress
                          }
                        , logGqlError "PostWithTagCb" err
                        )
                    )
                    (\data ->
                        ( { model
                            | posts =
                                model.posts
                                    |> Day.insert data.post.date data.post
                            , tags =
                                model.tags
                                    |> UD.map data.tagId
                                        (\t ->
                                            { t | posts = data.tagPosts }
                                        )
                            , inProgress = inProgress
                          }
                        , Cmd.none
                        )
                    )

        PostTagCb pair res ->
            let
                inProgress =
                    model.inProgress
                        |> (\p ->
                                { p
                                    | postTags =
                                        p.postTags
                                            |> List.filter ((/=) pair)
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
                    (\data ->
                        ( { model
                            | posts =
                                model.posts
                                    |> Day.update data.postDate
                                        (\p ->
                                            { p | tags = data.postTags }
                                        )
                            , tags =
                                model.tags
                                    |> UD.map data.tagId
                                        (\t ->
                                            { t | posts = data.tagPosts }
                                        )
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
                                    , inProgress = inProgress
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                ( { model
                                    | posts =
                                        model.posts
                                            |> Day.remove day
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
                                , passwordConfirm = ""
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
                            , posts = []
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
                , Data.nonce email
                    |> Task.attempt NonceCb
                )

            else
                ( { model | errors = [ "Invalid data" ] }
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
                        ( { model
                            | errors = parseErrors err
                            , inProgress = inProgress
                          }
                        , logGqlError "NonceCb" err
                        )
                    )
                    (\data ->
                        case data of
                            Types.Nonce n ->
                                ( { model
                                    | funnel = Types.WelcomeBack n
                                    , inProgress = inProgress
                                  }
                                , Cmd.none
                                )

                            Types.Guest n ->
                                ( { model
                                    | funnel = Types.GuestSignup n
                                    , inProgress = inProgress
                                  }
                                , Cmd.none
                                )

                            Types.Newbie ->
                                ( { model
                                    | funnel = Types.JoinUs
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

        FunnelCancel ->
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

        Buy ->
            ( { model
                | inProgress =
                    model.inProgress
                        |> (\p ->
                                { p
                                    | buy = True
                                }
                           )
              }
            , Ports.buy model.loginForm.email
            )

        SignupSubmit guest data ->
            let
                inProgress =
                    model.inProgress |> (\p -> { p | login = True })
            in
            if String.isEmpty model.loginForm.password then
                ( { model | errors = [ "empty field(s)" ] }
                , Cmd.none
                )

            else if model.loginForm.password /= model.loginForm.passwordConfirm then
                ( model
                , Cmd.none
                )

            else
                ( { model | errors = [], inProgress = inProgress }
                , Crypto.nonce
                    |> Task.andThen
                        (\nonce ->
                            Crypto.keys model.loginForm.password nonce
                                |> Task.andThen
                                    (\keys ->
                                        (if guest then
                                            Data.join keys.serverKey nonce data

                                         else
                                            Data.signup keys.serverKey nonce data
                                        )
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
            , model.auth
                |> unwrap
                    (tag.id
                        |> Task.succeed
                        |> Task.attempt TagDeleteCb
                    )
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
            , model.auth
                |> unwrap
                    ({ t | name = model.tagUpdate }
                        |> Task.succeed
                        |> Task.attempt TagUpdateCb
                    )
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

        RefreshCb cmd res ->
            res
                |> unpack
                    (\err ->
                        ( model
                        , logGqlError "RefreshCb" err
                        )
                    )
                    (Maybe.map2
                        (\auth token ->
                            let
                                newAuth =
                                    { auth | token = token }
                            in
                            ( { model
                                | auth = Just newAuth
                              }
                            , cmd newAuth
                            )
                        )
                        model.auth
                        >> Maybe.withDefault
                            ( { model | auth = Nothing }
                            , [ Ports.clearState ()
                              , goTo RouteHome
                              ]
                                |> Cmd.batch
                            )
                    )

        PostTagAttach day tagId ->
            let
                pair =
                    ( day, tagId )

                inProgress =
                    model.inProgress
                        |> (\p ->
                                { p
                                    | postTags =
                                        pair :: p.postTags
                                }
                           )
            in
            Day.get day model.posts
                |> unwrap
                    ( { model
                        | inProgress = inProgress
                      }
                    , model.auth
                        |> unwrap
                            (wait
                                |> Task.andThen
                                    (always <| randomTask Uuid.uuidGenerator)
                                |> Task.map
                                    (\id ->
                                        { post =
                                            { tags = [ Types.PostTag id tagId ]
                                            , date = day
                                            , body = Nothing
                                            , id = id
                                            }
                                        , tagId = tagId
                                        , tagPosts =
                                            model.tags
                                                |> UD.get tagId
                                                |> unwrap [] .posts
                                                |> (::) day
                                        }
                                            |> Ok
                                    )
                                |> Task.perform (PostWithTagCb pair)
                            )
                            (trip (Data.postCreateWithTag tagId day)
                                (PostWithTagCb pair)
                            )
                    )
                    (\post ->
                        ( { model
                            | inProgress = inProgress
                          }
                        , model.auth
                            |> unwrap
                                (wait
                                    |> Task.andThen
                                        (randomTask Uuid.uuidGenerator
                                            |> always
                                        )
                                    |> Task.map
                                        (\uuid ->
                                            { postDate = day
                                            , postTags =
                                                { id = uuid, tag = tagId } :: post.tags
                                            , tagId = tagId
                                            , tagPosts =
                                                model.tags
                                                    |> UD.get tagId
                                                    |> unwrap [] .posts
                                                    |> (::) day
                                            }
                                                |> Ok
                                        )
                                    |> Task.perform (PostTagCb pair)
                                )
                                (trip
                                    (Data.tagAttach post tagId)
                                    (PostTagCb pair)
                                )
                        )
                    )

        PostTagDetach day tagId ->
            let
                pair =
                    ( day, tagId )

                inProgress =
                    model.inProgress
                        |> (\p ->
                                { p
                                    | postTags =
                                        pair :: p.postTags
                                }
                           )
            in
            Day.get day model.posts
                |> Maybe.andThen
                    (\post ->
                        post.tags
                            |> List.Extra.find (.tag >> (==) tagId)
                            |> Maybe.map (\pt -> ( post, pt ))
                    )
                |> Maybe.map
                    (\( post, pt ) ->
                        ( { model
                            | inProgress = inProgress
                          }
                        , model.auth
                            |> unwrap
                                (wait
                                    |> Task.map
                                        (\_ ->
                                            { postDate = day
                                            , postTags =
                                                post.tags
                                                    |> List.filter
                                                        (\x -> x.id /= pt.id)
                                            , tagId = tagId
                                            , tagPosts =
                                                model.tags
                                                    |> UD.get tagId
                                                    |> unwrap []
                                                        (.posts
                                                            >> List.filter ((/=) day)
                                                        )
                                            }
                                                |> Ok
                                        )
                                    |> Task.perform (PostTagCb pair)
                                )
                                (trip
                                    (Data.tagDetach pt.id)
                                    (PostTagCb pair)
                                )
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

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

        LoginFormPasswordConfirmUpdate str ->
            ( { model
                | loginForm =
                    model.loginForm
                        |> (\f -> { f | passwordConfirm = str })
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

        VisibilityChange _ ->
            ( model
            , Cmd.none
            )

        TagSelect id ->
            ( { model
                | tag = Just id
              }
            , Cmd.batch
                [ goTo RouteTag
                , model.auth
                    |> unwrap Cmd.none
                        (trip (Data.fetchPostsByTag id)
                            PostsCb
                        )
                ]
            )

        TagDeselect ->
            ( { model
                | tag = Nothing
              }
            , if model.landscape then
                Cmd.none

              else
                goTo RouteTags
            )

        CellSelect d ->
            if model.current == Just d then
                if model.landscape then
                    ( { model
                        | postEditorBody =
                            Day.get d model.posts
                                |> Maybe.andThen .body
                                |> Maybe.withDefault ""
                        , postBeingEdited = True
                      }
                    , focusOnEditor
                    )

                else
                    ( model
                    , RouteDayDetail d
                        |> goTo
                    )

            else
                ( model
                , Types.RouteDay d
                    |> goTo
                )

        UrlChange route ->
            route
                |> unpack
                    (\err ->
                        ( model
                        , Ports.log err
                        )
                    )
                    (handleRoute model)

        JwtFailure cmd ->
            ( model
            , Data.refresh
                |> Task.attempt (RefreshCb cmd)
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
    case route of
        RouteHome ->
            ( { model
                | view = Types.ViewHome
                , def = Nothing
              }
            , Cmd.none
            )

        RouteTag ->
            ( { model | view = Types.ViewTags }, Cmd.none )

        RouteTags ->
            ( { model
                | view = Types.ViewTags
                , tag =
                    if model.landscape then
                        model.tag

                    else
                        Nothing
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

        RouteDayDetail d ->
            let
                txt =
                    Day.get d model.posts
                        |> Maybe.andThen .body

                startEdit =
                    isNothing txt
            in
            ( { model
                | postView = True
                , tagView = False
                , current = Just d
                , view = Types.ViewCalendar
                , postBeingEdited = startEdit
                , postEditorBody = txt |> Maybe.withDefault ""
              }
            , if startEdit then
                focusOnEditor

              else
                Cmd.none
            )

        RouteDayTags d ->
            ( { model
                | postView = True
                , tagView = True
                , current = Just d
                , view = Types.ViewCalendar
              }
            , Cmd.none
            )

        RouteDay d ->
            model.posts
                |> Day.get d
                |> routeDay model d


routeDay : Model -> Date -> Maybe Post -> ( Model, Cmd Msg )
routeDay model d pst =
    let
        startEdit =
            Just d == model.current && model.view == Types.ViewCalendar

        editorText =
            pst |> Maybe.andThen .body |> Maybe.withDefault ""
    in
    ( { model
        | postEditorBody = editorText
        , postCreateTags = []
        , postBeingEdited = startEdit
        , inProgress = model.inProgress |> (\p -> { p | post = False })
        , current = Just d
        , month = Calendar.getMonth d
        , year = Calendar.getYear d
        , postView = False
        , view = Types.ViewCalendar
      }
    , Cmd.batch
        [ model.auth
            |> unwrap Cmd.none
                (trip
                    (Data.fetchDay d)
                    (PostCb d)
                )
        , if model.landscape && startEdit then
            focusOnEditor

          else
            Cmd.none
        ]
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


trip : (Auth -> GqlTask a) -> (GqlResult a -> Msg) -> Auth -> Cmd Msg
trip task msg =
    task
        >> Task.attempt
            (unpack
                (\err ->
                    if codeCheck "invalid-jwt" err then
                        JwtFailure
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
