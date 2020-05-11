module CustomScalars exposing (Date, Id, Timestamptz, Uuid, codecs)

import Api.Scalar exposing (defaultCodecs)
import Date
import Json.Decode as JD
import Json.Encode as JE
import Uuid


type alias Date =
    Date.Date


type alias Id =
    Api.Scalar.Id


type alias Uuid =
    Uuid.Uuid


type alias Timestamptz =
    Api.Scalar.Timestamptz


codecs : Api.Scalar.Codecs Date Id Timestamptz Uuid
codecs =
    Api.Scalar.defineCodecs
        { codecDate =
            { encoder = Date.toIsoString >> JE.string
            , decoder =
                JD.string
                    |> JD.andThen
                        (Date.fromIsoString
                            >> Result.map JD.succeed
                            >> Result.withDefault (JD.fail "bad date")
                        )
            }
        , codecId = defaultCodecs.codecId
        , codecTimestamptz = defaultCodecs.codecTimestamptz
        , codecUuid =
            { encoder = Uuid.encode
            , decoder = Uuid.decoder
            }
        }
