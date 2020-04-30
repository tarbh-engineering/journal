module Crypto exposing (decrypt, encrypt, keys, nonce)

import Graphql.Http exposing (HttpError(..), RawError(..))
import Http
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import Types exposing (Cipher, GqlTask, Keys, Msg(..), Route(..), Sort(..), Status(..), View(..))


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


serviceWorkerRequest : String -> Value -> Decoder a -> GqlTask a
serviceWorkerRequest key body decoder =
    Http.task
        { method = "CRYPTO"
        , headers = []
        , url = "/" ++ key
        , body = Http.jsonBody body
        , resolver =
            Http.stringResolver
                (\response ->
                    case response of
                        Http.BadUrl_ url_ ->
                            Graphql.Http.BadUrl url_
                                |> HttpError
                                |> Err

                        Http.Timeout_ ->
                            Graphql.Http.Timeout
                                |> HttpError
                                |> Err

                        Http.NetworkError_ ->
                            Graphql.Http.NetworkError
                                |> HttpError
                                |> Err

                        Http.BadStatus_ metadata body_ ->
                            Graphql.Http.BadStatus metadata body_
                                |> HttpError
                                |> Err

                        Http.GoodStatus_ _ body_ ->
                            body_
                                |> Decode.decodeString decoder
                                |> Result.mapError
                                    (Graphql.Http.BadPayload
                                        >> HttpError
                                    )
                )
        , timeout = Nothing
        }
