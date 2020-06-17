module JwtScalar exposing (Jwt, decoder, encode, useToken)

import Graphql.Http exposing (Request)
import Json.Decode exposing (Decoder, Value)
import Json.Encode


type Jwt
    = Jwt String


decoder : Decoder Jwt
decoder =
    Json.Decode.string
        |> Json.Decode.map Jwt


encode : Jwt -> Value
encode (Jwt token) =
    Json.Encode.string token


useToken : Jwt -> Request a -> Request a
useToken (Jwt token) =
    Graphql.Http.withHeader "Authorization" ("Bearer " ++ token)
