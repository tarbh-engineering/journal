module Crypto exposing (decrypt, encrypt, keys, nonce)

import Boom
import Graphql.Http
import Helpers
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Types exposing (Cipher, GqlTask, Keys)


nonce : GqlTask String
nonce =
    serviceWorkerRequest "nonce" Encode.null Decode.string


keys : String -> String -> GqlTask Keys
keys password nonce_ =
    serviceWorkerRequest "keys"
        ([ ( "password", Encode.string password )
         , ( "nonce", Encode.string nonce_ )
         ]
            |> Encode.object
        )
        decodeKeys


encrypt : Value -> String -> GqlTask Cipher
encrypt key content =
    serviceWorkerRequest "encrypt"
        ([ ( "key", key )
         , ( "content", Encode.string content )
         ]
            |> Encode.object
        )
        decodeCipher


decrypt : Value -> Cipher -> GqlTask String
decrypt key cipher =
    serviceWorkerRequest "decrypt"
        ([ ( "key", key )
         , ( "ciphertext", Encode.string cipher.ciphertext )
         , ( "iv", Encode.string cipher.iv )
         ]
            |> Encode.object
        )
        Decode.string


decodeKeys : Decoder Keys
decodeKeys =
    Decode.map2 Keys
        (Decode.field "encryptionKey" Decode.value)
        (Decode.field "serverKey" Decode.string)


decodeCipher : Decoder Cipher
decodeCipher =
    Decode.map2 Cipher
        (Decode.field "iv" Decode.string)
        (Decode.field "ciphertext" Decode.string)


swFail : Graphql.Http.Error ()
swFail =
    Helpers.makeGqlError Boom.swFail


serviceWorkerRequest : String -> Value -> Decoder a -> GqlTask a
serviceWorkerRequest key body decoder =
    Http.task
        { method = "CRYPTO"
        , headers = []
        , url = "https://" ++ key
        , body = Http.jsonBody body
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.BadUrl_ _ ->
                            Err swFail

                        Http.Timeout_ ->
                            Err swFail

                        Http.NetworkError_ ->
                            Err swFail

                        Http.BadStatus_ _ _ ->
                            Err swFail

                        Http.GoodStatus_ _ body_ ->
                            body_
                                |> Decode.decodeString decoder
                                |> Result.mapError (always swFail)
                )
        , timeout = Nothing
        }
