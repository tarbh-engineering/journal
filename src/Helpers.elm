module Helpers exposing (extract, getStatus, jsonResolver, today)

import Date exposing (Date)
import Day exposing (DayDict)
import Graphql.Http exposing (HttpError(..), RawError(..))
import Http exposing (Resolver)
import Json.Decode as JD exposing (Decoder)
import Task exposing (Task)
import Time
import Types exposing (Status(..))


today : Task e Date
today =
    Task.map2
        Date.fromPosix
        Time.here
        Time.now


jsonResolver : Decoder a -> Resolver (Graphql.Http.Error ()) a
jsonResolver decoder =
    Http.stringResolver
        (\response ->
            case response of
                Http.BadUrl_ u ->
                    Graphql.Http.BadUrl u
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

                Http.BadStatus_ metadata x ->
                    Graphql.Http.BadStatus metadata x
                        |> HttpError
                        |> Err

                Http.GoodStatus_ _ body ->
                    body
                        |> JD.decodeString decoder
                        |> Result.mapError (Graphql.Http.BadPayload >> HttpError)
        )


getStatus : Date -> DayDict (Status a) -> Status a
getStatus d =
    Day.get d >> Maybe.withDefault Missing


extract : Status a -> Maybe a
extract s =
    case s of
        Found a ->
            Just a

        Loading ma ->
            ma

        Missing ->
            Nothing
