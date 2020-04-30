-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.Tag_update_column exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| update columns of table "tag"

  - Ciphertext - column name
  - Iv - column name

-}
type Tag_update_column
    = Ciphertext
    | Iv


list : List Tag_update_column
list =
    [ Ciphertext, Iv ]


decoder : Decoder Tag_update_column
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ciphertext" ->
                        Decode.succeed Ciphertext

                    "iv" ->
                        Decode.succeed Iv

                    _ ->
                        Decode.fail ("Invalid Tag_update_column type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : Tag_update_column -> String
toString enum =
    case enum of
        Ciphertext ->
            "ciphertext"

        Iv ->
            "iv"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Tag_update_column
fromString enumString =
    case enumString of
        "ciphertext" ->
            Just Ciphertext

        "iv" ->
            Just Iv

        _ ->
            Nothing
