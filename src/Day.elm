module Day exposing (DayDict, filter, get, insert, newDayDict, remove, toString, update, values)

import Calendar exposing (Date)
import Derberos.Date.Utils exposing (monthToNumber1)
import Dict exposing (Dict)
import Helpers exposing (padNum)


type DayDict a
    = DayDict (Dict Int a)


filter : (a -> Bool) -> DayDict a -> DayDict a
filter fn =
    apply <| Dict.filter (always fn)


update : Date -> (Maybe a -> Maybe a) -> DayDict a -> DayDict a
update d fn =
    apply <| Dict.update (Calendar.toMillis d) fn


get : Date -> DayDict a -> Maybe a
get k (DayDict dd) =
    Dict.get (Calendar.toMillis k) dd


insert : Date -> a -> DayDict a -> DayDict a
insert k v =
    apply <| Dict.insert (Calendar.toMillis k) v


remove : Date -> DayDict a -> DayDict a
remove k =
    apply <| Dict.remove (Calendar.toMillis k)


values : DayDict a -> List a
values (DayDict dd) =
    Dict.values dd


newDayDict : DayDict a
newDayDict =
    DayDict Dict.empty


toString : Date -> String
toString d =
    [ Calendar.getYear d
    , Calendar.getMonth d |> monthToNumber1
    , Calendar.getDay d
    ]
        |> List.map padNum
        |> String.join "-"


apply : (Dict Int a -> Dict Int b) -> DayDict a -> DayDict b
apply fn (DayDict dd) =
    fn dd
        |> DayDict
