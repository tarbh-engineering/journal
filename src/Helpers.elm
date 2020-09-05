module Helpers exposing (jsonResolver, now, padNum, today)

import Calendar exposing (Date)
import DateTime exposing (DateTime)
import Graphql.Http exposing (HttpError(..), RawError(..))
import Http exposing (Resolver)
import Json.Decode as JD exposing (Decoder)
import Task exposing (Task)
import Time


padNum : Int -> String
padNum =
    String.fromInt >> String.padLeft 2 '0'


now : Task e DateTime
now =
    Task.map2
        (\z t ->
            (Time.posixToMillis t + DateTime.getTimezoneOffset z t)
                |> Time.millisToPosix
                |> DateTime.fromPosix
        )
        Time.here
        Time.now


today : Task e Date
today =
    Task.map2
        (\z t ->
            (Time.posixToMillis t + DateTime.getTimezoneOffset z t)
                |> Time.millisToPosix
                |> Calendar.fromPosix
        )
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
