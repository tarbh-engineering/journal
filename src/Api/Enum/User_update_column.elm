-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.User_update_column exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| update columns of table "user"

  - Email - column name
  - Id - column name
  - Nonce - column name
  - Password - column name

-}
type User_update_column
    = Email
    | Id
    | Nonce
    | Password


list : List User_update_column
list =
    [ Email, Id, Nonce, Password ]


decoder : Decoder User_update_column
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "email" ->
                        Decode.succeed Email

                    "id" ->
                        Decode.succeed Id

                    "nonce" ->
                        Decode.succeed Nonce

                    "password" ->
                        Decode.succeed Password

                    _ ->
                        Decode.fail ("Invalid User_update_column type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : User_update_column -> String
toString enum =
    case enum of
        Email ->
            "email"

        Id ->
            "id"

        Nonce ->
            "nonce"

        Password ->
            "password"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe User_update_column
fromString enumString =
    case enumString of
        "email" ->
            Just Email

        "id" ->
            Just Id

        "nonce" ->
            Just Nonce

        "password" ->
            Just Password

        _ ->
            Nothing
