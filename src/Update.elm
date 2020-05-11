module Update exposing (new, update)

import Api.Enum.Post_constraint
import Api.Enum.Post_update_column
import Api.InputObject
import Api.Mutation
import Api.Object.Post
import Api.Object.Post_mutation_response
import Api.Object.Post_tag
import Api.Object.Post_tag_mutation_response
import Api.Object.Tag
import Api.Object.Tag_mutation_response
import Api.Query
import Browser
import Browser.Dom
import Browser.Events exposing (Visibility(..))
import Browser.Navigation as Navigation
import Crypto
import CustomScalars exposing (Uuid)
import Data exposing (fetchTags, graphqlEndpoint, ignoreParsedErrorData, mutate, postSelection, query, range, tagSelection)
import Date exposing (Date)
import Day exposing (DayDict)
import Derberos.Date.Utils exposing (getNextMonth, getPrevMonth)
import Dict
import Graphql.Http exposing (HttpError(..), RawError(..))
import Graphql.Http.GraphqlError
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Helpers
import Helpers.Parse
import Helpers.UuidDict as UD
import Json.Decode as JD
import Json.Encode as Encode
import Maybe.Extra exposing (unwrap)
import Ports
import Random
import Result.Extra exposing (unpack)
import Routing exposing (goTo)
import Task
import Time exposing (Month(..))
import Types exposing (Auth, GqlTask, Model, Msg(..), Post, PostView(..), Route(..), Sort(..), Status(..), Tag, View(..))
import Url
import Uuid


headOrFail : SelectionSet (List a) b -> SelectionSet a b
headOrFail =
    Graphql.SelectionSet.mapOrFail
        (List.head >> Result.fromMaybe "empty list response")


fetchPostsByTag : Uuid -> Auth -> GqlTask (List Post)
fetchPostsByTag id { key, token } =
    Api.Query.post_tag
        (\r ->
            { r
                | where_ =
                    Api.InputObject.buildPost_tag_bool_exp
                        (\r2 ->
                            { r2
                                | tag_id = equalToId id
                            }
                        )
                        |> Present
            }
        )
        (Api.Object.Post_tag.postBypostId postSelection)
        |> query token
        |> Task.andThen
            (List.map
                (\post ->
                    Crypto.decrypt key post.cipher
                        |> Task.map
                            (\str ->
                                { id = post.id
                                , body = str
                                , date = post.date
                                , tags = post.tags
                                }
                            )
                )
                >> Task.sequence
            )


editTag : Tag -> Auth -> GqlTask Tag
editTag { id, name } { token, key } =
    Crypto.encrypt key name
        |> Task.andThen
            (\res ->
                Api.Mutation.update_tag
                    (\r ->
                        { r
                            | set_ =
                                Api.InputObject.buildTag_set_input
                                    (\a ->
                                        { a
                                            | name = Present res.ciphertext
                                            , iv = Present res.iv
                                        }
                                    )
                                    |> Present
                        }
                    )
                    { where_ =
                        Api.InputObject.buildTag_bool_exp
                            (\a ->
                                { a
                                    | id = equalToId id
                                }
                            )
                    }
                    (Api.Object.Tag_mutation_response.returning tagSelection
                        |> Graphql.SelectionSet.mapOrFail
                            (List.head >> Result.fromMaybe "uh oh")
                    )
                    |> mutate token
            )
        |> Task.andThen
            (unwrap
                (Task.fail (makeGqlError "post doesn't exist"))
                (Data.tp key)
            )


deleteTag : Tag -> Auth -> GqlTask Uuid
deleteTag { id } { token } =
    Graphql.SelectionSet.map2 Tuple.pair
        (Api.Mutation.delete_post_tag
            { where_ =
                Api.InputObject.buildPost_tag_bool_exp
                    (\a ->
                        { a
                            | tag_id = equalToId id
                        }
                    )
            }
            (Graphql.SelectionSet.succeed ())
        )
        (Api.Mutation.delete_tag
            { where_ =
                Api.InputObject.buildTag_bool_exp
                    (\a ->
                        { a
                            | id = equalToId id
                        }
                    )
            }
            (Api.Object.Tag_mutation_response.returning Api.Object.Tag.id
                |> Graphql.SelectionSet.mapOrFail
                    (List.head >> Result.fromMaybe "uh oh")
            )
        )
        |> Graphql.SelectionSet.map Tuple.second
        |> mutate token
        |> Task.andThen
            (unwrap
                (makeGqlError "tag doesn't exist"
                    |> Task.fail
                )
                Task.succeed
            )


delete : Uuid -> Auth -> GqlTask Date
delete id { token } =
    Graphql.SelectionSet.map2 Tuple.pair
        (Api.Mutation.delete_post_tag
            { where_ =
                Api.InputObject.buildPost_tag_bool_exp
                    (\a ->
                        { a
                            | post_id = equalToId id
                        }
                    )
            }
            (Graphql.SelectionSet.succeed ())
        )
        (Api.Mutation.delete_post
            { where_ =
                Api.InputObject.buildPost_bool_exp
                    (\a ->
                        { a
                            | id = equalToId id
                        }
                    )
            }
            (Api.Object.Post_mutation_response.returning Api.Object.Post.date
                |> headOrFail
            )
        )
        |> Graphql.SelectionSet.map Tuple.second
        |> mutate token
        |> Task.andThen
            (unwrap
                (makeGqlError "post doesn't exist"
                    |> Task.fail
                )
                Task.succeed
            )


attachTag : Post -> Uuid -> Auth -> GqlTask Post
attachTag post tagId { token, key } =
    Api.Mutation.insert_post_tag
        identity
        { objects =
            [ Api.InputObject.buildPost_tag_insert_input
                (\r ->
                    { r
                        | post_id = Present post.id
                        , tag_id = Present tagId
                    }
                )
            ]
        }
        (Api.Object.Post_tag_mutation_response.returning
            (Api.Object.Post_tag.postBypostId
                postSelection
            )
            |> headOrFail
        )
        |> Graphql.SelectionSet.nonNullOrFail
        |> mutate token
        |> Task.andThen (Data.mp key)


removeTag : Uuid -> Auth -> GqlTask Post
removeTag tagId { token, key } =
    Api.Mutation.delete_post_tag
        { where_ =
            Api.InputObject.buildPost_tag_bool_exp
                (\a ->
                    { a
                        | tag_id = equalToId tagId
                    }
                )
        }
        (Api.Object.Post_tag_mutation_response.returning
            (Api.Object.Post_tag.postBypostId
                postSelection
            )
            |> headOrFail
        )
        |> Graphql.SelectionSet.nonNullOrFail
        |> mutate token
        |> Task.andThen (Data.mp key)


postUpdateBody : Uuid -> String -> Auth -> GqlTask Post
postUpdateBody id body { key, token } =
    Crypto.encrypt key body
        |> Task.andThen
            (\res ->
                Api.Mutation.update_post
                    (\r ->
                        { r
                            | set_ =
                                Api.InputObject.buildPost_set_input
                                    (\a ->
                                        { a
                                            | body =
                                                Present res.ciphertext
                                            , iv =
                                                Present res.iv
                                        }
                                    )
                                    |> Present
                        }
                    )
                    { where_ =
                        Api.InputObject.buildPost_bool_exp
                            (\a ->
                                { a
                                    | id = equalToId id
                                }
                            )
                    }
                    (Api.Object.Post_mutation_response.returning postSelection
                        |> headOrFail
                    )
                    |> mutate token
            )
        |> Task.andThen
            (unwrap
                (makeGqlError "post doesn't exist"
                    |> Task.fail
                )
                (Data.mp key)
            )


tagCreate : String -> Auth -> GqlTask Tag
tagCreate name { key, token } =
    Crypto.encrypt key name
        |> Task.andThen
            (\res ->
                Api.Mutation.insert_tag
                    identity
                    { objects =
                        [ Api.InputObject.buildTag_insert_input
                            (\r ->
                                { r
                                    | name =
                                        Present res.ciphertext
                                    , iv =
                                        Present res.iv
                                }
                            )
                        ]
                    }
                    (Api.Object.Tag_mutation_response.returning tagSelection
                        |> headOrFail
                    )
                    |> mutate token
            )
        |> Task.andThen
            (unwrap
                (makeGqlError "tag doesn't exist"
                    |> Task.fail
                )
                (Data.tp key)
            )


new : String -> List Uuid -> Date -> Auth -> GqlTask Post
new body tags d { key, token } =
    Crypto.encrypt key body
        |> Task.andThen
            (\res ->
                Api.Mutation.insert_post
                    (\r ->
                        { r
                            | on_conflict =
                                Api.InputObject.buildPost_on_conflict
                                    { constraint = Api.Enum.Post_constraint.Unique_post
                                    , update_columns = [ Api.Enum.Post_update_column.Body ]
                                    }
                                    identity
                                    |> Present
                        }
                    )
                    { objects =
                        [ Api.InputObject.buildPost_insert_input
                            (\r ->
                                { r
                                    | body =
                                        Present res.ciphertext
                                    , iv =
                                        Present res.iv
                                    , date =
                                        d
                                            |> Present
                                    , postTagsBypostId =
                                        if List.isEmpty tags then
                                            Absent

                                        else
                                            Api.InputObject.Post_tag_arr_rel_insert_input
                                                { data =
                                                    tags
                                                        |> List.map
                                                            (\id ->
                                                                Api.InputObject.buildPost_tag_insert_input
                                                                    (\r_ ->
                                                                        { r_
                                                                            | tag_id = Present id
                                                                        }
                                                                    )
                                                            )
                                                , on_conflict = Absent
                                                }
                                                |> Present
                                }
                            )
                        ]
                    }
                    (Api.Object.Post_mutation_response.returning postSelection
                        |> headOrFail
                    )
                    |> mutate token
            )
        |> Task.andThen
            (unwrap
                (makeGqlError "post doesn't exist"
                    |> Task.fail
                )
                (Data.mp key)
            )


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
                ( model, Cmd.none )

            else if model.auth == Nothing then
                ( model
                , Uuid.uuidGenerator
                    |> Random.map
                        (\uuid ->
                            { id = uuid
                            , body = model.postEditorBody
                            , tags = []
                            , date = d
                            }
                                |> Ok
                        )
                    |> Random.generate PostMutateCb
                )

            else
                ( { model | postSaveInProgress = True }
                , model.auth
                    |> unwrap Cmd.none
                        (new model.postEditorBody
                            model.postCreateTags
                            d
                            >> Task.attempt PostMutateCb
                        )
                )

        PostDelete id ->
            ( model
            , if model.auth == Nothing then
                Cmd.none

              else
                model.auth
                    |> unwrap Cmd.none
                        (delete id
                            >> Task.attempt PostDeleteCb
                        )
            )

        PostUpdateCancel ->
            ( { model
                | postBeingEdited = False
              }
            , Cmd.none
            )

        PostViewSet value ->
            ( { model
                | postView = value
              }
            , Cmd.none
            )

        PostUpdateStart body ->
            ( { model
                | postEditorBody = body
                , postBeingEdited = True
                , postSaveInProgress = False
              }
            , focusOnEditor
            )

        PostUpdateSubmit id ->
            if model.auth == Nothing then
                ( model
                , model.current
                    |> Maybe.andThen (\d -> Day.get d model.posts)
                    |> Maybe.andThen Helpers.extract
                    |> unwrap Cmd.none
                        (\p ->
                            if String.isEmpty model.postEditorBody then
                                Ok p.date
                                    |> Task.succeed
                                    |> Task.perform PostDeleteCb

                            else
                                Ok { p | body = model.postEditorBody }
                                    |> Task.succeed
                                    |> Task.perform PostMutateCb
                        )
                )

            else
                ( { model | postSaveInProgress = True }
                , model.auth
                    |> unwrap Cmd.none
                        (if String.isEmpty model.postEditorBody then
                            delete id
                                >> Task.attempt PostDeleteCb

                         else
                            postUpdateBody id model.postEditorBody
                                >> Task.attempt PostMutateCb
                        )
                )

        PostsCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model
                            | posts =
                                model.posts
                                    |> clearLoading
                            , online = not <| isNetworkError err
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
                                            Day.update v.date
                                                (unwrap
                                                    (Found v)
                                                    (\post ->
                                                        case post of
                                                            Missing ->
                                                                Found v

                                                            Loading _ ->
                                                                Found v

                                                            Found _ ->
                                                                Found v
                                                    )
                                                    >> Just
                                                )
                                        )
                                        model.posts
                                    |> clearLoading
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
                            , postSaveInProgress = False
                          }
                        , logGqlError "PostDeleteCb" err
                        )
                    )
                    (\date ->
                        ( { model
                            | posts =
                                Day.remove
                                    date
                                    model.posts
                            , postEditorBody = ""
                            , postSaveInProgress = False
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
                            , postSaveInProgress = False
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
                                        (Found post)
                            , postView = PostView
                            , postBeingEdited = False
                            , postSaveInProgress = False
                            , online = True
                          }
                        , Cmd.none
                        )
                    )

        PostCb day res ->
            res
                |> unpack
                    (\err ->
                        ( { model
                            | online = not <| isNetworkError err
                            , postSaveInProgress = False
                            , postBeingEdited = False
                          }
                        , logGqlError "PostCb" err
                        )
                    )
                    (\resPost ->
                        case resPost of
                            Just post ->
                                ( { model
                                    | posts =
                                        model.posts
                                            |> Day.insert
                                                post.date
                                                (Found post)
                                    , postBeingEdited = False
                                  }
                                , Cmd.none
                                )

                            Nothing ->
                                ( { model
                                    | posts =
                                        model.posts
                                            |> Day.remove day
                                    , postBeingEdited = False
                                  }
                                , Cmd.none
                                )
                    )

        TagCreateCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model | online = not <| isNetworkError err }
                        , logGqlError "TagCreateCb" err
                        )
                    )
                    (\tag ->
                        ( { model
                            | tags =
                                UD.insert tag.id tag model.tags
                            , tagCreateName = ""
                          }
                        , Cmd.none
                        )
                    )

        TagCreateSubmit ->
            ( model
            , if String.isEmpty model.tagCreateName then
                Cmd.none

              else if model.auth == Nothing then
                Uuid.uuidGenerator
                    |> Random.map
                        (\uuid ->
                            { id = uuid
                            , name = model.tagCreateName
                            , count = 0
                            }
                                |> Ok
                        )
                    |> Random.generate TagCreateCb

              else
                model.auth
                    |> unwrap Cmd.none
                        (tagCreate model.tagCreateName
                            >> Task.attempt TagCreateCb
                        )
            )

        BodyUpdate str ->
            ( { model | postEditorBody = str }, Cmd.none )

        AuthCb res ->
            res
                |> unpack
                    (\err ->
                        ( { model
                            | errors = parseErrors err
                            , online = not <| isNetworkError err
                          }
                        , logGqlError "AuthCb" err
                        )
                    )
                    (\auth ->
                        let
                            now =
                                Task.map2
                                    Date.fromPosix
                                    Time.here
                                    Time.now
                        in
                        ( { model
                            | auth = Just auth
                            , view = ViewCalendar
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
                            , Ports.saveAuth auth
                            , fetchTags auth
                                |> Task.attempt TagsCb
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

        PostCreateTagToggle tag ->
            ( { model
                | postCreateTags =
                    model.postCreateTags
                        |> toggle tag.id
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

            else
                ( { model | errors = [] }
                , getNonce email
                    |> Task.attempt NonceCb
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
                              }
                            , Cmd.none
                            )

                        else if check "no-purchase" err then
                            ( { model
                                | funnel = Types.JoinUs
                              }
                            , Cmd.none
                            )

                        else
                            ( { model
                                | errors = parseErrors err
                                , online = not <| isNetworkError err
                              }
                            , logGqlError "NonceCb" err
                            )
                    )
                    (\nonce ->
                        ( { model
                            | funnel = Types.WelcomeBack nonce
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
                            login email keys.serverKey
                                |> Task.map
                                    (\token ->
                                        { key = keys.encryptionKey
                                        , token = token
                                        , email = email
                                        }
                                    )
                        )
                    |> Task.attempt AuthCb
                )

        Change ->
            ( { model | funnel = Types.Hello }, Cmd.none )

        Logout ->
            ( { model
                | auth = Nothing
                , posts = Day.newDayDict
                , tags = UD.empty
                , loginForm =
                    { email = ""
                    , password = ""
                    , passwordVisible = False
                    }
                , funnel = Types.Hello
                , view = ViewHome
              }
            , Cmd.batch [ Ports.clearAuth (), goTo RouteHome ]
            )

        Buy annual ->
            ( model, Ports.buy { email = model.loginForm.email, annual = annual } )

        SignupSubmit txt ->
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
                                        signup keys.serverKey nonce txt
                                            |> Task.map
                                                (\token ->
                                                    { key = keys.encryptionKey
                                                    , token = token
                                                    , email = "ok"
                                                    }
                                                )
                                    )
                        )
                    |> Task.attempt AuthCb
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
                        (deleteTag tag >> Task.attempt TagDeleteCb)
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
                        (editTag t
                            >> Task.attempt TagUpdateCb
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

        PostTagToggle post tag ->
            ( model
            , if model.auth == Nothing then
                { post
                    | tags =
                        post.tags
                            |> toggle tag.id
                }
                    |> Task.succeed
                    |> Task.map Just
                    |> Task.attempt (PostCb post.date)

              else if List.member tag.id post.tags then
                model.auth
                    |> unwrap Cmd.none
                        (removeTag tag.id
                            >> Task.map Just
                            >> Task.attempt (PostCb post.date)
                        )

              else
                model.auth
                    |> unwrap Cmd.none
                        (attachTag post tag.id
                            >> Task.map Just
                            >> Task.attempt (PostCb post.date)
                        )
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
            , model.auth
                |> unwrap Cmd.none
                    (fetchPostsByTag id
                        >> Task.attempt PostsCb
                    )
            )

        UrlChange route ->
            case route of
                RouteToday ->
                    ( model
                    , Task.map2
                        Date.fromPosix
                        Time.here
                        Time.now
                        |> Task.perform (RouteDay >> NavigateTo)
                    )

                RouteHome ->
                    ( { model
                        | view = ViewHome
                      }
                    , Cmd.none
                    )

                RouteTags ->
                    ( { model
                        | view = ViewTags
                        , tags =
                            if model.auth == Nothing then
                                model.tags
                                    |> UD.values
                                    |> List.map
                                        (\t ->
                                            { t
                                                | count =
                                                    model.posts
                                                        |> Day.values
                                                        |> List.filterMap Helpers.extract
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
                            (fetchTags >> Task.attempt TagsCb)
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

                RouteDay d ->
                    model.posts
                        |> Helpers.getStatus d
                        |> (\data ->
                                let
                                    newPost =
                                        case data of
                                            Missing ->
                                                Loading Nothing

                                            Loading ma ->
                                                Loading ma

                                            Found a ->
                                                Loading (Just a)

                                    shouldFocusOnEditor =
                                        case data of
                                            Missing ->
                                                True

                                            Loading _ ->
                                                False

                                            Found _ ->
                                                model.postBeingEdited

                                    editorText =
                                        case data of
                                            Missing ->
                                                ""

                                            Loading ma ->
                                                ma
                                                    |> unwrap "" .body

                                            Found a ->
                                                a.body
                                in
                                ( { model
                                    --| view = ViewPost d
                                    | postView = PostView
                                    , posts =
                                        if model.auth == Nothing then
                                            model.posts

                                        else
                                            model.posts
                                                |> Day.insert d newPost
                                    , postEditorBody = editorText
                                    , postCreateTags = []
                                    , postBeingEdited = False
                                    , postSaveInProgress = False
                                    , current = Just d
                                    , month = Date.month d
                                    , year = Date.year d
                                  }
                                , Cmd.batch
                                    [ model.auth
                                        |> unwrap Cmd.none
                                            (Data.fetchDay d
                                                >> Task.attempt (PostCb d)
                                            )
                                    , if shouldFocusOnEditor then
                                        focusOnEditor

                                      else
                                        Cmd.none
                                    ]
                                )
                           )

                NotFound ->
                    ( model
                      --, Debug.log "bad route" route
                    , Cmd.none
                      --, Cmd.batch
                      --[ Ports.log "redirecting..."
                      --, goTo RouteHome
                      --]
                    )

        NavigateTo route ->
            ( model, goTo route )


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


signup : String -> String -> String -> GqlTask String
signup pw nonce txt =
    Api.Mutation.signup
        { nonce = nonce
        , password = pw
        , txt = txt
        }
        |> Graphql.Http.mutationRequest graphqlEndpoint
        |> Graphql.Http.toTask
        |> Task.mapError ignoreParsedErrorData


login : String -> String -> GqlTask String
login email pw =
    Api.Mutation.login
        { email = email
        , password = pw
        }
        |> Graphql.Http.mutationRequest graphqlEndpoint
        |> Graphql.Http.toTask
        |> Task.mapError ignoreParsedErrorData


getNonce : String -> GqlTask String
getNonce email =
    Api.Query.nonce
        { email = email
        }
        |> Graphql.Http.queryRequest graphqlEndpoint
        |> Graphql.Http.toTask
        |> Task.mapError ignoreParsedErrorData


logGqlError : String -> Graphql.Http.Error a -> Cmd msg
logGqlError tag err =
    Ports.log (tag ++ ":\n" ++ Helpers.Parse.gqlError err)


equalToId : Uuid -> OptionalArgument Api.InputObject.Uuid_comparison_exp
equalToId id =
    Present <|
        Api.InputObject.buildUuid_comparison_exp
            (\r ->
                { r | eq_ = Present id }
            )


makeGqlError : String -> Graphql.Http.Error ()
makeGqlError str =
    Graphql.Http.GraphqlError
        (Graphql.Http.GraphqlError.UnparsedData Encode.null)
        [ { message = str
          , locations = Nothing
          , details = Dict.empty
          }
        ]


toggle : a -> List a -> List a
toggle v ls =
    if List.member v ls then
        List.filter ((/=) v) ls

    else
        v :: ls


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
