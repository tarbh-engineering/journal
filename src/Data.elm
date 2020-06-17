module Data exposing (attachTag, check, delete, deleteTag, editTag, fetchDay, fetchPostsByTag, graphqlEndpoint, ignoreParsedErrorData, login, logout, mp, mutate, new, nonce, postSelection, postUpdateBody, query, range, refresh, removeTag, signup, tagCreate, tagSelection, tags, tp)

import Api.Enum.Post_constraint
import Api.Enum.Post_update_column
import Api.InputObject
import Api.Mutation
import Api.Object
import Api.Object.Post
import Api.Object.Post_mutation_response
import Api.Object.Post_tag
import Api.Object.Post_tag_aggregate
import Api.Object.Post_tag_aggregate_fields
import Api.Object.Post_tag_mutation_response
import Api.Object.Tag
import Api.Object.Tag_mutation_response
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


mp : Value -> PostRaw -> GqlTask Post
mp key post =
    Crypto.decrypt key post.cipher
        |> Task.map
            (\str ->
                { id = post.id
                , body = str
                , date = post.date
                , tags = post.tags
                }
            )


tp : Value -> TagRaw -> GqlTask Tag
tp key tag =
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
                    (mp key >> Task.map Just)
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
            (List.map (mp key)
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
            Api.Object.Tag.name
        )
        Api.Object.Tag.id
        (Api.Object.Tag.postTagsBytagId_aggregate identity
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
        (Graphql.SelectionSet.map2 Cipher
            Api.Object.Post.iv
            Api.Object.Post.body
        )
        Api.Object.Post.id
        (Api.Object.Post.postTagsBypostId identity
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
                (tp key)
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
                (tp key)
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
        |> Task.andThen (mp key)


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
        |> Task.andThen (mp key)


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
                (mp key)
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
                (tp key)
            )


new : String -> List Uuid -> Date -> Auth -> GqlTask Post
new body tags_ d { key, token } =
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
                                        if List.isEmpty tags_ then
                                            Absent

                                        else
                                            Api.InputObject.Post_tag_arr_rel_insert_input
                                                { data =
                                                    tags_
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
                (mp key)
            )


headOrFail : SelectionSet (List a) b -> SelectionSet a b
headOrFail =
    Graphql.SelectionSet.mapOrFail
        (List.head >> Result.fromMaybe "empty list response")


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
