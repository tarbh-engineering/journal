module Routing exposing (goTo, parseSignup, router)

import Calendar exposing (Date)
import Day
import Iso8601
import Ports
import Types exposing (Route)
import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, s, string, top)


goTo : Route -> Cmd msg
goTo route =
    (case route of
        Types.RouteCalendar ->
            absolute [ "calendar" ] []

        Types.RouteToday ->
            absolute [ "today" ] []

        Types.RouteDay day ->
            absolute [ "calendar", Day.toString day ] []

        Types.RouteDayDetail day ->
            absolute [ "calendar", Day.toString day, "view" ] []

        Types.RouteTags ->
            absolute [ "tags" ] []

        Types.RouteTag ->
            absolute [ "tags", "detail" ] []

        Types.RouteSettings ->
            absolute [ "settings" ] []

        Types.RouteHome ->
            absolute [] []
    )
        |> Ports.pushUrl


routes : List (Parser (Maybe Route -> a) a)
routes =
    [ map (Just Types.RouteHome) top
    , map (Just Types.RouteToday) (s "today")
    , map (Just Types.RouteCalendar) (s "calendar")
    , map (Just Types.RouteSettings) (s "settings")
    , map (Just Types.RouteTags) (s "tags")
    , map (Just Types.RouteTag) (s "tags" </> s "detail")
    , map (parseDay Types.RouteDay) (s "calendar" </> string)
    , map (parseDay Types.RouteDayDetail) (s "calendar" </> string </> s "view")
    ]


parseSignup : Url -> Maybe String
parseSignup =
    Url.Parser.parse
        (s "signup" </> Url.Parser.string)


parseDay : (Date -> Route) -> String -> Maybe Route
parseDay r =
    Iso8601.toTime
        >> Result.map Calendar.fromPosix
        >> Result.toMaybe
        >> Maybe.map r


router : String -> Result String Route
router href =
    href
        |> Url.fromString
        |> Maybe.andThen (parse (oneOf routes))
        |> Maybe.andThen identity
        |> Result.fromMaybe ("Bad route:\n" ++ href)
