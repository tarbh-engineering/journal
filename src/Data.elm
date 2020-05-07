module Data exposing (fetchDay, fetchTags, graphqlEndpoint, ignoreParsedErrorData, mutate, postSelection, query, range, serviceWorkerRequest, tagSelection)

import Api.InputObject
import Api.Object
import Api.Object.Post
import Api.Object.Post_tag
import Api.Object.Post_tag_aggregate
import Api.Object.Post_tag_aggregate_fields
import Api.Object.Tag
import Api.Query
import Api.Scalar exposing (Uuid(..))
import Date exposing (Date)
import Graphql.Http exposing (HttpError(..), RawError(..))
import Graphql.Operation
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Maybe.Extra exposing (unwrap)
import Task
import Types exposing (Auth, GqlTask, Msg(..), Post, Route(..), ServiceWorkerRequest(..), Sort(..), Status(..), Tag, View(..))


graphqlEndpoint : String
graphqlEndpoint =
    "/graphql"


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
                    (\post ->
                        serviceWorkerRequest
                            (Decrypt
                                { key = key, content = post.body }
                            )
                            Decode.string
                            |> Task.map (\str -> Just { post | body = str })
                    )
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
            (List.map
                (\post ->
                    serviceWorkerRequest
                        (Decrypt
                            { key = key, content = post.body }
                        )
                        Decode.string
                        |> Task.map (\str -> { post | body = str })
                )
                >> Task.sequence
            )


ignoreParsedErrorData : Graphql.Http.Error a -> Graphql.Http.Error ()
ignoreParsedErrorData =
    Graphql.Http.mapError (always ())


tagSelection : SelectionSet Tag Api.Object.Tag
tagSelection =
    Graphql.SelectionSet.map3 Tag
        Api.Object.Tag.name
        Api.Object.Tag.id
        (Api.Object.Tag.postTagsBytagId_aggregate identity
            (Api.Object.Post_tag_aggregate.aggregate
                (Api.Object.Post_tag_aggregate_fields.count identity
                    |> Graphql.SelectionSet.map (Maybe.withDefault 0)
                )
                |> Graphql.SelectionSet.map (Maybe.withDefault 0)
            )
        )


postSelection : SelectionSet Post Api.Object.Post
postSelection =
    Graphql.SelectionSet.map4 Post
        Api.Object.Post.date
        Api.Object.Post.body
        Api.Object.Post.id
        (Api.Object.Post.postTagsBypostId identity
            Api.Object.Post_tag.tag_id
        )


fetchTags : Auth -> GqlTask (List Tag)
fetchTags { key, token } =
    Api.Query.tag
        identity
        tagSelection
        |> query token
        |> Task.andThen
            (List.map
                (\tag ->
                    serviceWorkerRequest
                        (Decrypt
                            { key = key, content = tag.name }
                        )
                        Decode.string
                        |> Task.map (\str -> { tag | name = str })
                )
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


serviceWorkerRequest : ServiceWorkerRequest -> Decoder a -> GqlTask a
serviceWorkerRequest req decoder =
    let
        ( url, body ) =
            case req of
                GenerateNonce ->
                    ( "nonce", Http.emptyBody )

                GenerateKeys { password, nonce } ->
                    ( "keys"
                    , [ ( "password", Encode.string password )
                      , ( "nonce", Encode.string nonce )
                      ]
                        |> Encode.object
                        |> Http.jsonBody
                    )

                Decrypt { key, content } ->
                    ( "decrypt"
                    , [ ( "key", key )
                      , ( "content", Encode.string content )
                      ]
                        |> Encode.object
                        |> Http.jsonBody
                    )

                Encrypt { key, content } ->
                    ( "encrypt"
                    , [ ( "key", key )
                      , ( "content", Encode.string content )
                      ]
                        |> Encode.object
                        |> Http.jsonBody
                    )
    in
    Http.task
        { method = "CRYPTO"
        , headers = []
        , url = "/" ++ url
        , body = body
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.BadUrl_ url_ ->
                            Graphql.Http.BadUrl url_
                                |> HttpError
                                |> Err

                        Http.Timeout_ ->
                            Graphql.Http.Timeout
                                |> HttpError
                                |> Err

                        Http.NetworkError_ ->
                            Graphql.Http.NetworkError
                                |> HttpError
                                |> Err

                        Http.BadStatus_ metadata body_ ->
                            Graphql.Http.BadStatus metadata body_
                                |> HttpError
                                |> Err

                        Http.GoodStatus_ _ body_ ->
                            body_
                                |> Decode.decodeString decoder
                                |> Result.mapError
                                    (Graphql.Http.BadPayload
                                        >> HttpError
                                    )
                )
        , timeout = Nothing
        }
