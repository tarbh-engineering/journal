module CustomScalars exposing (Date, Id, Jwt, Timestamptz, Uuid, codecs)

import Api.Scalar exposing (defaultCodecs)
import Calendar
import DateTime
import Iso8601
import Json.Decode as JD
import Json.Encode as JE
import JwtScalar
import Time
import Uuid


type alias Timestamptz =
    DateTime.DateTime


type alias Date =
    Calendar.Date


type alias Id =
    Api.Scalar.Id


type alias Uuid =
    Uuid.Uuid


type alias Jwt =
    JwtScalar.Jwt


codecs : Api.Scalar.Codecs Date Id Jwt Timestamptz Uuid
codecs =
    Api.Scalar.defineCodecs
        { codecDate =
            { encoder =
                Calendar.toMillis
                    >> Time.millisToPosix
                    >> Iso8601.fromTime
                    >> JE.string
            , decoder =
                JD.string
                    |> JD.andThen
                        (Iso8601.toTime
                            >> Result.map (Calendar.fromPosix >> JD.succeed)
                            >> Result.withDefault (JD.fail "bad date")
                        )
            }
        , codecId = defaultCodecs.codecId
        , codecTimestamptz =
            { encoder =
                DateTime.toPosix >> Iso8601.fromTime >> JE.string
            , decoder =
                Iso8601.decoder
                    |> JD.map DateTime.fromPosix
            }
        , codecJwt =
            { encoder = JwtScalar.encode
            , decoder = JwtScalar.decoder
            }
        , codecUuid =
            { encoder = Uuid.encode
            , decoder = Uuid.decoder
            }
        }
