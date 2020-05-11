-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.Purchase_constraint exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| unique or primary key constraints on table "purchase"

  - Purchase\_email\_key - unique or primary key constraint
  - Purchase\_pkey - unique or primary key constraint

-}
type Purchase_constraint
    = Purchase_email_key
    | Purchase_pkey


list : List Purchase_constraint
list =
    [ Purchase_email_key, Purchase_pkey ]


decoder : Decoder Purchase_constraint
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "purchase_email_key" ->
                        Decode.succeed Purchase_email_key

                    "purchase_pkey" ->
                        Decode.succeed Purchase_pkey

                    _ ->
                        Decode.fail ("Invalid Purchase_constraint type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : Purchase_constraint -> String
toString enum =
    case enum of
        Purchase_email_key ->
            "purchase_email_key"

        Purchase_pkey ->
            "purchase_pkey"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Purchase_constraint
fromString enumString =
    case enumString of
        "purchase_email_key" ->
            Just Purchase_email_key

        "purchase_pkey" ->
            Just Purchase_pkey

        _ ->
            Nothing
