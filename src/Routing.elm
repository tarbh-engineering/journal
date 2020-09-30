module Routing exposing (goTo, parseBoot, router)

import Calendar exposing (Date)
import Day
import Dict
import Iso8601
import Maybe.Extra
import Ports
import Types exposing (BootAction, Route)
import Url exposing (Url)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, parse, s, string, top)
import Url.Parser.Query as Q


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


parseBoot : Url -> Maybe BootAction
parseBoot =
    Url.Parser.parse
        (top
            <?> Q.string "signup"
            <?> Q.enum "payment_result"
                    (Dict.fromList
                        [ ( "true", True )
                        , ( "false", False )
                        ]
                    )
            |> map
                (\sn res ->
                    [ sn
                        |> Maybe.map Types.BootSignup
                    , res
                        |> Maybe.map
                            (\r ->
                                if r then
                                    Types.BootPaymentSuccess

                                else
                                    Types.BootPaymentFail
                            )
                    ]
                        |> Maybe.Extra.values
                        |> List.head
                )
        )
        >> Maybe.andThen identity


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
