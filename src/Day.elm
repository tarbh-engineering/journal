module Day exposing (DayDict, filter, get, getMonthDays, getWeek, insert, map, newDay, newDayDict, remove, shift, toString, update, values)

import Date exposing (Date)
import Derberos.Date.Utils exposing (monthToNumber1, numberOfDaysInMonth)
import Dict exposing (Dict)
import List.Nonempty as Nonempty exposing (Nonempty)
import Time exposing (Month(..), Weekday(..))


type DayDict a
    = DayDict (Dict Int a)


getWeek : Date -> Nonempty Date
getWeek d =
    let
        start =
            if Date.weekday d == Time.Mon then
                d

            else
                Date.floor Date.Monday d

        end =
            Date.add Date.Days 7 start
    in
    Date.range Date.Day
        1
        start
        end
        |> Nonempty.fromList
        |> Maybe.withDefault
            (Nonempty.fromElement (newDay 1 Time.Jan 1))


getMonthDays : Int -> Month -> List Date
getMonthDays year month =
    numberOfDaysInMonth year month
        |> List.range 1
        |> List.map
            (\monthDay ->
                newDay monthDay month year
            )


shift : Int -> Date -> Date
shift n =
    Date.add Date.Days n


map : (Date -> a -> b) -> DayDict a -> DayDict b
map fn (DayDict dd) =
    dd
        |> Dict.toList
        |> List.map
            (\( rd, p ) ->
                rd
                    |> Date.fromRataDie
                    |> (\d -> ( rd, fn d p ))
            )
        |> Dict.fromList
        |> DayDict


filter : (a -> Bool) -> DayDict a -> DayDict a
filter fn =
    apply <| Dict.filter (always fn)


update : Date -> (Maybe a -> Maybe a) -> DayDict a -> DayDict a
update d fn =
    apply <| Dict.update (Date.toRataDie d) fn


get : Date -> DayDict a -> Maybe a
get k (DayDict dd) =
    Dict.get (Date.toRataDie k) dd


insert : Date -> a -> DayDict a -> DayDict a
insert k v =
    apply <| Dict.insert (Date.toRataDie k) v


remove : Date -> DayDict a -> DayDict a
remove k =
    apply <| Dict.remove (Date.toRataDie k)


values : DayDict a -> List a
values (DayDict dd) =
    Dict.values dd


newDayDict : DayDict a
newDayDict =
    DayDict Dict.empty


padNum : Int -> String
padNum =
    String.fromInt >> String.padLeft 2 '0'


newDay : Int -> Month -> Int -> Date
newDay day month year =
    Date.fromCalendarDate year month day


toString : Date -> String
toString d =
    [ Date.year d
    , Date.month d |> monthToNumber1
    , Date.day d
    ]
        |> List.map padNum
        |> String.join "-"


apply : (Dict Int a -> Dict Int b) -> DayDict a -> DayDict b
apply fn (DayDict dd) =
    fn dd
        |> DayDict
