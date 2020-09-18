module Helpers exposing (now, padNum, today)

import Calendar exposing (Date)
import DateTime exposing (DateTime)
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
