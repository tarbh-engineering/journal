-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.Purchase_update_column exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| update columns of table "purchase"

  - Checkout\_session\_id - column name
  - Created\_at - column name
  - Email - column name
  - Id - column name
  - Updated\_at - column name

-}
type Purchase_update_column
    = Checkout_session_id
    | Created_at
    | Email
    | Id
    | Updated_at


list : List Purchase_update_column
list =
    [ Checkout_session_id, Created_at, Email, Id, Updated_at ]


decoder : Decoder Purchase_update_column
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "checkout_session_id" ->
                        Decode.succeed Checkout_session_id

                    "created_at" ->
                        Decode.succeed Created_at

                    "email" ->
                        Decode.succeed Email

                    "id" ->
                        Decode.succeed Id

                    "updated_at" ->
                        Decode.succeed Updated_at

                    _ ->
                        Decode.fail ("Invalid Purchase_update_column type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : Purchase_update_column -> String
toString enum =
    case enum of
        Checkout_session_id ->
            "checkout_session_id"

        Created_at ->
            "created_at"

        Email ->
            "email"

        Id ->
            "id"

        Updated_at ->
            "updated_at"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Purchase_update_column
fromString enumString =
    case enumString of
        "checkout_session_id" ->
            Just Checkout_session_id

        "created_at" ->
            Just Created_at

        "email" ->
            Just Email

        "id" ->
            Just Id

        "updated_at" ->
            Just Updated_at

        _ ->
            Nothing
