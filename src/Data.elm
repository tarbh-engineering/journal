module Data exposing (check, fetchDay, fetchPostsByTag, graphqlEndpoint, ignoreParsedErrorData, login, logout, mutate, nonce, postCreate, postCreateWithTag, postDelete, postSelection, postUpdateBody, query, range, refresh, signup, tagAttach, tagCreate, tagDelete, tagDetach, tagSelection, tagUpdate, tags)

import Api.InputObject
import Api.Mutation
import Api.Object
import Api.Object.Post
import Api.Object.Post_tag
import Api.Object.Post_tag_aggregate
import Api.Object.Post_tag_aggregate_fields
import Api.Object.Tag
import Api.Query
import Crypto
import CustomScalars exposing (Jwt, Uuid)
import Date exposing (Date)
import Dict
import Graphql.Http exposing (HttpError(..), RawError(..))
import Graphql.Http.GraphqlError
import Graphql.Operation
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode exposing (Value)
import Json.Encode as JE
import JwtScalar exposing (useToken)
import Maybe.Extra exposing (unwrap)
import Task
import Types exposing (Auth, Cipher, GqlTask, Msg(..), Post, PostRaw, Route(..), Sort(..), Status(..), Tag, TagRaw, View(..))


graphqlEndpoint : String
graphqlEndpoint =
    "/graphql"


postDecrypt : Value -> PostRaw -> GqlTask Post
postDecrypt key post =
    post.cipher
        |> unwrap
            (Task.succeed Nothing)
            (Crypto.decrypt key
                >> Task.map Just
            )
        |> Task.map
            (\val ->
                { id = post.id
                , body = val
                , date = post.date
                , tags = post.tags
                }
            )


tagDecrypt : Value -> TagRaw -> GqlTask Tag
tagDecrypt key tag =
    Crypto.decrypt key tag.cipher
        |> Task.map
            (\str ->
                { id = tag.id
                , name = str
                , count = tag.count
                }
            )


rangeQuery : Date -> Date -> Api.InputObject.Post_bool_exp
rangeQuery start end =
    Api.InputObject.buildPost_bool_exp
        (\r1 ->
            { r1
                | and_ =
                    [ Api.InputObject.buildPost_bool_exp
                        (\r2 ->
                            { r2
                                | date =
                                    Api.InputObject.buildDate_comparison_exp
                                        (\r3 ->
                                            { r3
                                                | gte_ =
                                                    start
                                                        |> Present
                                            }
                                        )
                                        |> Present
                            }
                        )
                        |> Just
                    , Api.InputObject.buildPost_bool_exp
                        (\r2 ->
                            { r2
                                | date =
                                    Api.InputObject.buildDate_comparison_exp
                                        (\r3 ->
                                            { r3
                                                | lte_ =
                                                    end
                                                        |> Present
                                            }
                                        )
                                        |> Present
                            }
                        )
                        |> Just
                    ]
                        |> Present
            }
        )


fetchDay : Date -> Auth -> GqlTask (Maybe Post)
fetchDay day { key, token } =
    Api.Query.post
        (\r ->
            { r
                | where_ =
                    Api.InputObject.buildPost_bool_exp
                        (\r2 ->
                            { r2
                                | date =
                                    Api.InputObject.buildDate_comparison_exp
                                        (\r3 ->
                                            { r3
                                                | eq_ =
                                                    day
                                                        |> Present
                                            }
                                        )
                                        |> Present
                            }
                        )
                        |> Present
            }
        )
        postSelection
        |> query token
        |> Task.andThen
            (List.head
                >> unwrap (Task.succeed Nothing)
                    (postDecrypt key >> Task.map Just)
            )


range : Date -> Date -> Auth -> GqlTask (List Post)
range start end { key, token } =
    Api.Query.post
        (\r ->
            { r
                | where_ =
                    rangeQuery start end
                        |> Present
            }
        )
        postSelection
        |> query token
        |> Task.andThen
            (List.map (postDecrypt key)
                >> Task.sequence
            )


ignoreParsedErrorData : Graphql.Http.Error a -> Graphql.Http.Error ()
ignoreParsedErrorData =
    Graphql.Http.mapError (always ())


tagSelection : SelectionSet TagRaw Api.Object.Tag
tagSelection =
    Graphql.SelectionSet.map3 TagRaw
        (Graphql.SelectionSet.map2 Cipher
            Api.Object.Tag.iv
            Api.Object.Tag.ciphertext
        )
        Api.Object.Tag.id
        (Api.Object.Tag.post_tags_aggregate identity
            (Api.Object.Post_tag_aggregate.aggregate
                (Api.Object.Post_tag_aggregate_fields.count identity
                    |> Graphql.SelectionSet.map (Maybe.withDefault 0)
                )
                |> Graphql.SelectionSet.map (Maybe.withDefault 0)
            )
        )


postSelection : SelectionSet Types.PostRaw Api.Object.Post
postSelection =
    Graphql.SelectionSet.map4 Types.PostRaw
        Api.Object.Post.date
        (Graphql.SelectionSet.map2
            (Maybe.map2 Cipher)
            Api.Object.Post.iv
            Api.Object.Post.ciphertext
        )
        Api.Object.Post.id
        (Api.Object.Post.post_tags identity
            Api.Object.Post_tag.tag_id
        )


refresh : GqlTask (Maybe Jwt)
refresh =
    Api.Query.refresh
        |> Graphql.Http.queryRequest graphqlEndpoint
        |> Graphql.Http.toTask
        |> Task.mapError ignoreParsedErrorData


check : String -> String -> GqlTask Bool
check iv ciph =
    Api.Query.check
        { iv = iv
        , ciph = ciph
        }
        |> Graphql.Http.queryRequest graphqlEndpoint
        |> Graphql.Http.toTask
        |> Task.mapError ignoreParsedErrorData


logout : GqlTask Bool
logout =
    Api.Mutation.logout
        |> Graphql.Http.mutationRequest graphqlEndpoint
        |> Graphql.Http.toTask
        |> Task.mapError ignoreParsedErrorData


tags : Auth -> GqlTask (List Tag)
tags { key, token } =
    Api.Query.tag
        identity
        tagSelection
        |> query token
        |> Task.andThen
            (List.map
                (tagDecrypt key)
                >> Task.sequence
            )


signup : String -> String -> String -> String -> GqlTask Jwt
signup pw nonce_ iv ciph =
    Api.Mutation.signup
        { nonce = nonce_
        , password = pw
        , iv = iv
        , ciph = ciph
        }
        |> Graphql.Http.mutationRequest graphqlEndpoint
        |> Graphql.Http.toTask
        |> Task.mapError ignoreParsedErrorData


login : String -> String -> GqlTask Jwt
login email pw =
    Api.Mutation.login
        { email = email
        , password = pw
        }
        |> Graphql.Http.mutationRequest graphqlEndpoint
        |> Graphql.Http.toTask
        |> Task.mapError ignoreParsedErrorData


nonce : String -> GqlTask String
nonce email =
    Api.Query.nonce
        { email = email
        }
        |> Graphql.Http.queryRequest graphqlEndpoint
        |> Graphql.Http.toTask
        |> Task.mapError ignoreParsedErrorData


equalToId : Uuid -> OptionalArgument Api.InputObject.Uuid_comparison_exp
equalToId id =
    Present <|
        Api.InputObject.buildUuid_comparison_exp
            (\r ->
                { r | eq_ = Present id }
            )


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
        (Api.Object.Post_tag.post postSelection)
        |> query token
        |> Task.andThen
            (List.map
                (postDecrypt key)
                >> Task.sequence
            )


tagUpdate : Tag -> Auth -> GqlTask Tag
tagUpdate { id, name } { token, key } =
    Crypto.encrypt key name
        |> Task.andThen
            (\res ->
                Api.Mutation.update_tag_by_pk
                    (\r ->
                        { r
                            | set_ =
                                Api.InputObject.buildTag_set_input
                                    (\a ->
                                        { a
                                            | ciphertext = Present res.ciphertext
                                            , iv = Present res.iv
                                        }
                                    )
                                    |> Present
                        }
                    )
                    { pk_columns = { id = id } }
                    tagSelection
                    |> mutate token
            )
        |> Task.andThen
            (unwrap
                (Task.fail (makeGqlError "post doesn't exist"))
                (tagDecrypt key)
            )


makeGqlError : String -> Graphql.Http.Error ()
makeGqlError str =
    Graphql.Http.GraphqlError
        (Graphql.Http.GraphqlError.UnparsedData JE.null)
        [ { message = str
          , locations = Nothing
          , details = Dict.empty
          }
        ]


tagDelete : Tag -> Auth -> GqlTask Uuid
tagDelete { id } { token } =
    Api.Mutation.delete_tag_by_pk
        { id = id }
        Api.Object.Tag.id
        |> mutate token
        |> Task.andThen
            (unwrap
                (makeGqlError "tag doesn't exist"
                    |> Task.fail
                )
                Task.succeed
            )


postDelete : Uuid -> Auth -> GqlTask Date
postDelete id { token } =
    Api.Mutation.delete_post_by_pk
        { id = id }
        Api.Object.Post.date
        |> mutate token
        |> Task.andThen
            (unwrap
                (makeGqlError "post doesn't exist"
                    |> Task.fail
                )
                Task.succeed
            )


tagAttach : Post -> Uuid -> Auth -> GqlTask Post
tagAttach post tagId { token, key } =
    Api.Mutation.insert_post_tag_one
        { object =
            Api.InputObject.buildPost_tag_insert_input
                (\r ->
                    { r
                        | post_id = Present post.id
                        , tag_id = Present tagId
                    }
                )
        }
        (Api.Object.Post_tag.post postSelection)
        |> Graphql.SelectionSet.nonNullOrFail
        |> mutate token
        |> Task.andThen (postDecrypt key)


tagDetach : Uuid -> Auth -> GqlTask Post
tagDetach tagId { token, key } =
    Api.Mutation.delete_post_tag_by_pk
        { id = tagId }
        (Api.Object.Post_tag.post postSelection)
        |> Graphql.SelectionSet.nonNullOrFail
        |> mutate token
        |> Task.andThen (postDecrypt key)


postUpdateBody : Uuid -> String -> Auth -> GqlTask Post
postUpdateBody id body { key, token } =
    Crypto.encrypt key body
        |> Task.andThen
            (\res ->
                Api.Mutation.update_post_by_pk
                    (\r ->
                        { r
                            | set_ =
                                Api.InputObject.buildPost_set_input
                                    (\a ->
                                        { a
                                            | ciphertext =
                                                Present res.ciphertext
                                            , iv =
                                                Present res.iv
                                        }
                                    )
                                    |> Present
                        }
                    )
                    { pk_columns = { id = id } }
                    postSelection
                    |> mutate token
            )
        |> Task.andThen
            (unwrap
                (makeGqlError "post doesn't exist"
                    |> Task.fail
                )
                (postDecrypt key)
            )


tagCreate : String -> Auth -> GqlTask Tag
tagCreate name { key, token } =
    Crypto.encrypt key name
        |> Task.andThen
            (\res ->
                Api.Mutation.insert_tag_one
                    identity
                    { object =
                        Api.InputObject.buildTag_insert_input
                            (\r ->
                                { r
                                    | ciphertext =
                                        Present res.ciphertext
                                    , iv =
                                        Present res.iv
                                }
                            )
                    }
                    tagSelection
                    |> mutate token
            )
        |> Task.andThen
            (unwrap
                (makeGqlError "tag doesn't exist"
                    |> Task.fail
                )
                (tagDecrypt key)
            )


postCreate : String -> List Uuid -> Date -> Auth -> GqlTask Post
postCreate body tags_ d { key, token } =
    Crypto.encrypt key body
        |> Task.andThen
            (\res ->
                Api.Mutation.insert_post_one
                    identity
                    { object =
                        Api.InputObject.buildPost_insert_input
                            (\r ->
                                { r
                                    | ciphertext =
                                        Present res.ciphertext
                                    , iv =
                                        Present res.iv
                                    , date =
                                        d
                                            |> Present
                                    , post_tags =
                                        if List.isEmpty tags_ then
                                            Absent

                                        else
                                            Api.InputObject.buildPost_tag_arr_rel_insert_input
                                                { data =
                                                    tags_
                                                        |> List.map
                                                            (\id ->
                                                                Api.InputObject.buildPost_tag_insert_input
                                                                    (\r__ ->
                                                                        { r__
                                                                            | tag_id = Present id
                                                                        }
                                                                    )
                                                            )
                                                }
                                                |> Present
                                }
                            )
                    }
                    postSelection
                    |> mutate token
            )
        |> Task.andThen
            (unwrap
                (makeGqlError "post doesn't exist"
                    |> Task.fail
                )
                (postDecrypt key)
            )


postCreateWithTag : Uuid -> Date -> Auth -> GqlTask Post
postCreateWithTag tag d { key, token } =
    Api.Mutation.insert_post_one
        identity
        { object =
            Api.InputObject.buildPost_insert_input
                (\r ->
                    { r
                        | ciphertext = Null
                        , iv = Null
                        , date = Present d
                        , post_tags =
                            Api.InputObject.buildPost_tag_arr_rel_insert_input
                                { data =
                                    [ Api.InputObject.buildPost_tag_insert_input
                                        (\r__ ->
                                            { r__
                                                | tag_id = Present tag
                                            }
                                        )
                                    ]
                                }
                                |> Present
                    }
                )
        }
        postSelection
        |> mutate token
        |> Task.andThen
            (unwrap
                (makeGqlError "post doesn't exist"
                    |> Task.fail
                )
                (postDecrypt key)
            )


mutate : Jwt -> SelectionSet a Graphql.Operation.RootMutation -> GqlTask a
mutate token =
    Graphql.Http.mutationRequest graphqlEndpoint
        >> useToken token
        >> Graphql.Http.toTask
        >> Task.mapError ignoreParsedErrorData


query : Jwt -> SelectionSet a Graphql.Operation.RootQuery -> GqlTask a
query token =
    Graphql.Http.queryRequest graphqlEndpoint
        >> useToken token
        >> Graphql.Http.toTask
        >> Task.mapError ignoreParsedErrorData
