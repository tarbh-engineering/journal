-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Scalar exposing (Codecs, Date(..), Id(..), Jwt(..), Timestamptz(..), Uuid(..), defaultCodecs, defineCodecs, unwrapCodecs, unwrapEncoder)

import Graphql.Codec exposing (Codec)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Date
    = Date String


type Id
    = Id String


type Jwt
    = Jwt String


type Timestamptz
    = Timestamptz String


type Uuid
    = Uuid String


defineCodecs :
    { codecDate : Codec valueDate
    , codecId : Codec valueId
    , codecJwt : Codec valueJwt
    , codecTimestamptz : Codec valueTimestamptz
    , codecUuid : Codec valueUuid
    }
    -> Codecs valueDate valueId valueJwt valueTimestamptz valueUuid
defineCodecs definitions =
    Codecs definitions


unwrapCodecs :
    Codecs valueDate valueId valueJwt valueTimestamptz valueUuid
    ->
        { codecDate : Codec valueDate
        , codecId : Codec valueId
        , codecJwt : Codec valueJwt
        , codecTimestamptz : Codec valueTimestamptz
        , codecUuid : Codec valueUuid
        }
unwrapCodecs (Codecs unwrappedCodecs) =
    unwrappedCodecs


unwrapEncoder getter (Codecs unwrappedCodecs) =
    (unwrappedCodecs |> getter |> .encoder) >> Graphql.Internal.Encode.fromJson


type Codecs valueDate valueId valueJwt valueTimestamptz valueUuid
    = Codecs (RawCodecs valueDate valueId valueJwt valueTimestamptz valueUuid)


type alias RawCodecs valueDate valueId valueJwt valueTimestamptz valueUuid =
    { codecDate : Codec valueDate
    , codecId : Codec valueId
    , codecJwt : Codec valueJwt
    , codecTimestamptz : Codec valueTimestamptz
    , codecUuid : Codec valueUuid
    }


defaultCodecs : RawCodecs Date Id Jwt Timestamptz Uuid
defaultCodecs =
    { codecDate =
        { encoder = \(Date raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Date
        }
    , codecId =
        { encoder = \(Id raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Id
        }
    , codecJwt =
        { encoder = \(Jwt raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Jwt
        }
    , codecTimestamptz =
        { encoder = \(Timestamptz raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Timestamptz
        }
    , codecUuid =
        { encoder = \(Uuid raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Uuid
        }
    }
