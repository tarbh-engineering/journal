module Routing exposing (goTo, router)

import Api.Scalar exposing (Id(..), Uuid(..))
import Date exposing (Date)
import Day
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

        RouteTags ->
            absolute [ "tags" ] []

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
    , map (Just RouteStats) (s "stats")
    , map (parseDay RouteDay) (s "calendar" </> string)
    ]


parseDay : (Date -> Route) -> String -> Maybe Route
parseDay r =
    Date.fromIsoString
        >> Result.toMaybe
        >> Maybe.map r


router : Url -> Maybe Route
router =
    parse (oneOf routes)
        >> Maybe.andThen identity
