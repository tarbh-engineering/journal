module Helpers exposing (makeGqlError, now, padNum, today)

import Calendar exposing (Date)
import DateTime exposing (DateTime)
import Dict
import Graphql.Http
import Graphql.Http.GraphqlError
import Json.Encode as JE
import Task exposing (Task)
import Time


padNum : Int -> String
padNum =
    String.fromInt >> String.padLeft 2 '0'


now : Task e DateTime
now =
    Task.map2
        (\z t ->
            (Time.posixToMillis t + DateTime.getTimezoneOffset z t)
                |> Time.millisToPosix
                |> DateTime.fromPosix
        )
        Time.here
        Time.now


today : Task e Date
today =
    Task.map2
        (\z t ->
            (Time.posixToMillis t + DateTime.getTimezoneOffset z t)
                |> Time.millisToPosix
                |> Calendar.fromPosix
        )
        Time.here
        Time.now


makeGqlError : String -> Graphql.Http.Error ()
makeGqlError str =
    Graphql.Http.GraphqlError
        (Graphql.Http.GraphqlError.UnparsedData JE.null)
        [ { message = "code err"
          , locations = Nothing
          , details =
                [ ( "err"
                  , [ ( "code", JE.string str ) ]
                        |> JE.object
                  )
                ]
                    |> Dict.fromList
          }
        ]
