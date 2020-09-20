module Day exposing (DayDict, get, insert, newDayDict, remove, toString, update)

import Calendar exposing (Date)
import Derberos.Date.Utils exposing (monthToNumber1)
import Dict exposing (Dict)
import Helpers exposing (padNum)


type DayDict a
    = DayDict (Dict Int a)


get : Date -> DayDict a -> Maybe a
get k (DayDict dd) =
    Dict.get (Calendar.toMillis k) dd


insert : Date -> a -> DayDict a -> DayDict a
insert k v =
    Dict.insert (Calendar.toMillis k) v
        |> apply


remove : Date -> DayDict a -> DayDict a
remove k =
    Dict.remove (Calendar.toMillis k)
        |> apply


update : Date -> (a -> a) -> DayDict a -> DayDict a
update d fn =
    Dict.update (Calendar.toMillis d) (Maybe.map fn)
        |> apply


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
