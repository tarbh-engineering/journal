module Data exposing (check, fetchDay, fetchTags, graphqlEndpoint, ignoreParsedErrorData, mp, mutate, postSelection, query, range, tagSelection, tp)

import Api.InputObject
import Api.Object
import Api.Object.Post
import Api.Object.Post_tag
import Api.Object.Post_tag_aggregate
import Api.Object.Post_tag_aggregate_fields
import Api.Object.Tag
import Api.Query
import Api.Scalar exposing (Uuid(..))
import Crypto
import Date exposing (Date)
import Graphql.Http exposing (HttpError(..), RawError(..))
import Graphql.Operation
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode exposing (Value)
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


check : String -> GqlTask Bool
check txt =
    Api.Query.check
        { txt = txt
        }
        |> Graphql.Http.queryRequest graphqlEndpoint
        |> Graphql.Http.toTask
        |> Task.mapError ignoreParsedErrorData


fetchTags : Auth -> GqlTask (List Tag)
fetchTags { key, token } =
    Api.Query.tag
        identity
        tagSelection
        |> query token
        |> Task.andThen
            (List.map
                (tp key)
                >> Task.sequence
            )


mutate : String -> SelectionSet a Graphql.Operation.RootMutation -> GqlTask a
mutate token =
    Graphql.Http.mutationRequest graphqlEndpoint
        >> Graphql.Http.withHeader "authorization" ("Bearer " ++ token)
        >> Graphql.Http.toTask
        >> Task.mapError ignoreParsedErrorData


query : String -> SelectionSet a Graphql.Operation.RootQuery -> GqlTask a
query token =
    Graphql.Http.queryRequest graphqlEndpoint
        >> Graphql.Http.withHeader "authorization" ("Bearer " ++ token)
        >> Graphql.Http.toTask
        >> Task.mapError ignoreParsedErrorData
