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
            absolute [ "app", "calendar" ] []

        Types.RouteDay day ->
            absolute [ "app", "calendar", Day.toString day ] []

        Types.RouteDayDetail day ->
            absolute [ "app", "calendar", Day.toString day, "view" ] []

        Types.RouteDayTags day ->
            absolute [ "app", "calendar", Day.toString day, "tags" ] []

        Types.RouteTags ->
            absolute [ "app", "tags" ] []

        Types.RouteTag ->
            absolute [ "app", "tags", "detail" ] []

        Types.RouteSettings ->
            absolute [ "app", "settings" ] []

        Types.RouteHome ->
            absolute [] []
    )
        |> Ports.pushUrl


routes : List (Parser (Maybe Route -> a) a)
routes =
    [ map (Just Types.RouteHome) top
    , map (Just Types.RouteCalendar) (s "app" </> s "calendar")
    , map (Just Types.RouteSettings) (s "app" </> s "settings")
    , map (Just Types.RouteTags) (s "app" </> s "tags")
    , map (Just Types.RouteTag) (s "app" </> s "tags" </> s "detail")
    , map (parseDay Types.RouteDay) (s "app" </> s "calendar" </> string)
    , map (parseDay Types.RouteDayDetail) (s "app" </> s "calendar" </> string </> s "view")
    , map (parseDay Types.RouteDayTags) (s "app" </> s "calendar" </> string </> s "tags")
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
