-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.Post_tag_constraint exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| unique or primary key constraints on table "post\_tag"

  - Post\_tag\_pkey - unique or primary key constraint

-}
type Post_tag_constraint
    = Post_tag_pkey


list : List Post_tag_constraint
list =
    [ Post_tag_pkey ]


decoder : Decoder Post_tag_constraint
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "post_tag_pkey" ->
                        Decode.succeed Post_tag_pkey

                    _ ->
                        Decode.fail ("Invalid Post_tag_constraint type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : Post_tag_constraint -> String
toString enum =
    case enum of
        Post_tag_pkey ->
            "post_tag_pkey"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Post_tag_constraint
fromString enumString =
    case enumString of
        "post_tag_pkey" ->
            Just Post_tag_pkey

        _ ->
            Nothing
