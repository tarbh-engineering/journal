-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.Post_constraint exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| unique or primary key constraints on table "post"

  - Post\_pkey - unique or primary key constraint
  - Unique\_post - unique or primary key constraint

-}
type Post_constraint
    = Post_pkey
    | Unique_post


list : List Post_constraint
list =
    [ Post_pkey, Unique_post ]


decoder : Decoder Post_constraint
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "post_pkey" ->
                        Decode.succeed Post_pkey

                    "unique_post" ->
                        Decode.succeed Unique_post

                    _ ->
                        Decode.fail ("Invalid Post_constraint type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : Post_constraint -> String
toString enum =
    case enum of
        Post_pkey ->
            "post_pkey"

        Unique_post ->
            "unique_post"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Post_constraint
fromString enumString =
    case enumString of
        "post_pkey" ->
            Just Post_pkey

        "unique_post" ->
            Just Unique_post

        _ ->
            Nothing
