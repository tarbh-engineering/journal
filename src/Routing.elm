module Routing exposing (goTo, router)

import Api.Scalar exposing (Id(..), Uuid(..))
import Date exposing (Date)
import Day
import Derberos.Date.Utils exposing (monthToNumber1, numberToMonth)
import Maybe.Extra exposing (unwrap)
import Ports
import Types exposing (Route(..))
import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), Parser, int, map, oneOf, parse, s, string, top)


goTo : Route -> Cmd msg
goTo route =
    (case route of
        NotFound ->
            absolute [] []

        RouteToday ->
            absolute [ "today" ] []

        RouteDay day ->
            absolute [ "day", Day.toString day ] []

        RouteWeek day ->
            absolute [ "week", Day.toString day ] []

        RouteYear year ->
            absolute [ "year", String.fromInt year ] []

        RouteTagPosts a ->
            absolute [ "tags", "id" ] []

        RouteTags ->
            absolute [ "tags" ] []

        RouteSettings ->
            absolute [ "settings" ] []

        RouteMonth month year ->
            absolute
                [ "month"
                , month
                    |> monthToNumber1
                    |> String.fromInt
                    |> String.padLeft 2 '0'
                , year
                    |> String.fromInt
                ]
                []

        RouteHome ->
            absolute [] []

        RouteLogin ->
            absolute [ "login" ] []
    )
        |> Ports.pushUrl


routes : List (Parser (Route -> a) a)
routes =
    [ map RouteHome top
    , map RouteToday (s "today")
    , map RouteSettings (s "settings")
    , map RouteLogin (s "login")
    , map RouteTags (s "tags")
    , map (parseDay RouteDay) (s "day" </> string)
    , map (parseDay RouteWeek) (s "week" </> string)
    , map RouteYear (s "year" </> int)

    --, map (Uuid >> RouteTagPosts) (s "tags" </> string)
    , map
        (\m y ->
            m
                - 1
                |> numberToMonth
                |> unwrap NotFound (\m_ -> RouteMonth m_ y)
        )
        (s "month" </> int </> int)
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
