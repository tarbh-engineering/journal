module Helpers exposing (extract, getStatus)

import Date exposing (Date)
import Day exposing (DayDict)
import Types exposing (Status(..))


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
