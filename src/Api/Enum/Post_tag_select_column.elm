-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Enum.Post_tag_select_column exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| select columns of table "post\_tag"

  - Id - column name
  - Post\_id - column name
  - Tag\_id - column name

-}
type Post_tag_select_column
    = Id
    | Post_id
    | Tag_id


list : List Post_tag_select_column
list =
    [ Id, Post_id, Tag_id ]


decoder : Decoder Post_tag_select_column
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "id" ->
                        Decode.succeed Id

                    "post_id" ->
                        Decode.succeed Post_id

                    "tag_id" ->
                        Decode.succeed Tag_id

                    _ ->
                        Decode.fail ("Invalid Post_tag_select_column type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : Post_tag_select_column -> String
toString enum =
    case enum of
        Id ->
            "id"

        Post_id ->
            "post_id"

        Tag_id ->
            "tag_id"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Post_tag_select_column
fromString enumString =
    case enumString of
        "id" ->
            Just Id

        "post_id" ->
            Just Post_id

        "tag_id" ->
            Just Tag_id

        _ ->
            Nothing
