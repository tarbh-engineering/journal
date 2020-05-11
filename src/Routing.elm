module Routing exposing (goTo, router)

import Api.Scalar exposing (Id(..), Uuid(..))
import Date exposing (Date)
import Day
import Maybe.Extra exposing (unwrap)
import Ports
import Types exposing (Route(..))
import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)


goTo : Route -> Cmd msg
goTo route =
    (case route of
        NotFound ->
            absolute [] []

        RouteCalendar ->
            absolute [ "calendar" ] []

        RouteToday ->
            absolute [ "today" ] []

        RouteDay day ->
            absolute [ "day", Day.toString day ] []

        RouteTags ->
            absolute [ "tags" ] []

        RouteSettings ->
            absolute [ "settings" ] []

        RouteHome ->
            absolute [] []
    )
        |> Ports.pushUrl


routes : List (Parser (Route -> a) a)
routes =
    [ map RouteHome top
    , map RouteToday (s "today")
    , map RouteCalendar (s "calendar")
    , map RouteSettings (s "settings")
    , map RouteTags (s "tags")
    , map (parseDay RouteDay) (s "day" </> string)
    ]


parseDay : (Date -> Route) -> String -> Route
parseDay r =
    Date.fromIsoString
        >> Result.toMaybe
        >> unwrap NotFound r


router : Url -> Route
router =
    parse (oneOf routes)
        >> Maybe.withDefault NotFound
