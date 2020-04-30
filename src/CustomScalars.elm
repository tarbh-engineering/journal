module CustomScalars exposing (Date, Id, Jwt, Uuid, codecs)

import Api.Scalar exposing (defaultCodecs)
import Date
import Json.Decode as JD
import Json.Encode as JE
import JwtScalar
import Uuid


type alias Date =
    Date.Date


type alias Id =
    Api.Scalar.Id


type alias Uuid =
    Uuid.Uuid


type alias Jwt =
    JwtScalar.Jwt


codecs : Api.Scalar.Codecs Date Id Jwt Uuid
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
        , codecJwt =
            { encoder = JwtScalar.encode
            , decoder = JwtScalar.decoder
            }
        , codecUuid =
            { encoder = Uuid.encode
            , decoder = Uuid.decoder
            }
        }
