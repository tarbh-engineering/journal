module Routing exposing (goTo, router)

import Api.Scalar exposing (Id(..), Uuid(..))
import Calendar exposing (Date)
import Day
import Iso8601
import Ports
import Types exposing (Route(..))
import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)


goTo : Route -> Cmd msg
goTo route =
    (case route of
        RouteCalendar ->
            absolute [ "calendar" ] []

        RouteToday ->
            absolute [ "today" ] []

        RouteDay day ->
            absolute [ "calendar", Day.toString day ] []

        RouteDayDetail day ->
            absolute [ "calendar", Day.toString day, "view" ] []

        RouteTags ->
            absolute [ "tags" ] []

        RouteTag ->
            absolute [ "tags", "detail" ] []

        RouteStats ->
            absolute [ "stats" ] []

        RouteSettings ->
            absolute [ "settings" ] []

        RouteHome ->
            absolute [] []
    )
        |> Ports.pushUrl


routes : List (Parser (Maybe Route -> a) a)
routes =
    [ map (Just RouteHome) top
    , map (Just RouteToday) (s "today")
    , map (Just RouteCalendar) (s "calendar")
    , map (Just RouteSettings) (s "settings")
    , map (Just RouteTags) (s "tags")
    , map (Just RouteTag) (s "tags" </> s "detail")
    , map (Just RouteStats) (s "stats")
    , map (parseDay RouteDay) (s "calendar" </> string)
    , map (parseDay RouteDayDetail) (s "calendar" </> string </> s "view")
    ]


parseDay : (Date -> Route) -> String -> Maybe Route
parseDay r =
    Iso8601.toTime
        >> Result.map Calendar.fromPosix
        >> Result.toMaybe
        >> Maybe.map r


router : Url -> Maybe Route
router =
    parse (oneOf routes)
        >> Maybe.andThen identity
