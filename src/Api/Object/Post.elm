-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Post exposing (..)

import Api.Enum.Post_tag_select_column
import Api.InputObject
import Api.Interface
import Api.Object
import Api.Scalar
import Api.Union
import CustomScalars
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


ciphertext : SelectionSet (Maybe String) Api.Object.Post
ciphertext =
    Object.selectionForField "(Maybe String)" "ciphertext" [] (Decode.string |> Decode.nullable)


date : SelectionSet CustomScalars.Date Api.Object.Post
date =
    Object.selectionForField "CustomScalars.Date" "date" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecDate |> .decoder)


id : SelectionSet CustomScalars.Uuid Api.Object.Post
id =
    Object.selectionForField "CustomScalars.Uuid" "id" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecUuid |> .decoder)


iv : SelectionSet (Maybe String) Api.Object.Post
iv =
    Object.selectionForField "(Maybe String)" "iv" [] (Decode.string |> Decode.nullable)


type alias PostTagsOptionalArguments =
    { distinct_on : OptionalArgument (List Api.Enum.Post_tag_select_column.Post_tag_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Api.InputObject.Post_tag_order_by)
    , where_ : OptionalArgument Api.InputObject.Post_tag_bool_exp
    }


{-| An array relationship

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
post_tags :
    (PostTagsOptionalArguments -> PostTagsOptionalArguments)
    -> SelectionSet decodesTo Api.Object.Post_tag
    -> SelectionSet (List decodesTo) Api.Object.Post
post_tags fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum Api.Enum.Post_tag_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (Api.InputObject.encodePost_tag_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ Api.InputObject.encodePost_tag_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "post_tags" optionalArgs object_ (identity >> Decode.list)


type alias PostTagsAggregateOptionalArguments =
    { distinct_on : OptionalArgument (List Api.Enum.Post_tag_select_column.Post_tag_select_column)
    , limit : OptionalArgument Int
    , offset : OptionalArgument Int
    , order_by : OptionalArgument (List Api.InputObject.Post_tag_order_by)
    , where_ : OptionalArgument Api.InputObject.Post_tag_bool_exp
    }


{-| An aggregated array relationship

  - distinct\_on - distinct select on columns
  - limit - limit the number of rows returned
  - offset - skip the first n rows. Use only with order\_by
  - order\_by - sort the rows by one or more columns
  - where\_ - filter the rows returned

-}
post_tags_aggregate :
    (PostTagsAggregateOptionalArguments -> PostTagsAggregateOptionalArguments)
    -> SelectionSet decodesTo Api.Object.Post_tag_aggregate
    -> SelectionSet decodesTo Api.Object.Post
post_tags_aggregate fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum Api.Enum.Post_tag_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (Api.InputObject.encodePost_tag_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ Api.InputObject.encodePost_tag_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "post_tags_aggregate" optionalArgs object_ identity
