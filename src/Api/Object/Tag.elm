-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Tag exposing (..)

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


created_at : SelectionSet CustomScalars.Timestamptz Api.Object.Tag
created_at =
    Object.selectionForField "CustomScalars.Timestamptz" "created_at" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder)


id : SelectionSet CustomScalars.Uuid Api.Object.Tag
id =
    Object.selectionForField "CustomScalars.Uuid" "id" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecUuid |> .decoder)


iv : SelectionSet String Api.Object.Tag
iv =
    Object.selectionForField "String" "iv" [] Decode.string


name : SelectionSet String Api.Object.Tag
name =
    Object.selectionForField "String" "name" [] Decode.string


type alias PostTagsBytagIdOptionalArguments =
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
postTagsBytagId :
    (PostTagsBytagIdOptionalArguments -> PostTagsBytagIdOptionalArguments)
    -> SelectionSet decodesTo Api.Object.Post_tag
    -> SelectionSet (List decodesTo) Api.Object.Tag
postTagsBytagId fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum Api.Enum.Post_tag_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (Api.InputObject.encodePost_tag_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ Api.InputObject.encodePost_tag_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "postTagsBytagId" optionalArgs object_ (identity >> Decode.list)


type alias PostTagsBytagIdAggregateOptionalArguments =
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
postTagsBytagId_aggregate :
    (PostTagsBytagIdAggregateOptionalArguments -> PostTagsBytagIdAggregateOptionalArguments)
    -> SelectionSet decodesTo Api.Object.Post_tag_aggregate
    -> SelectionSet decodesTo Api.Object.Tag
postTagsBytagId_aggregate fillInOptionals object_ =
    let
        filledInOptionals =
            fillInOptionals { distinct_on = Absent, limit = Absent, offset = Absent, order_by = Absent, where_ = Absent }

        optionalArgs =
            [ Argument.optional "distinct_on" filledInOptionals.distinct_on (Encode.enum Api.Enum.Post_tag_select_column.toString |> Encode.list), Argument.optional "limit" filledInOptionals.limit Encode.int, Argument.optional "offset" filledInOptionals.offset Encode.int, Argument.optional "order_by" filledInOptionals.order_by (Api.InputObject.encodePost_tag_order_by |> Encode.list), Argument.optional "where" filledInOptionals.where_ Api.InputObject.encodePost_tag_bool_exp ]
                |> List.filterMap identity
    in
    Object.selectionForCompositeField "postTagsBytagId_aggregate" optionalArgs object_ identity


updated_at : SelectionSet CustomScalars.Timestamptz Api.Object.Tag
updated_at =
    Object.selectionForField "CustomScalars.Timestamptz" "updated_at" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder)


{-| An object relationship
-}
userByuserId :
    SelectionSet decodesTo Api.Object.User
    -> SelectionSet decodesTo Api.Object.Tag
userByuserId object_ =
    Object.selectionForCompositeField "userByuserId" [] object_ identity


user_id : SelectionSet CustomScalars.Uuid Api.Object.Tag
user_id =
    Object.selectionForField "CustomScalars.Uuid" "user_id" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecUuid |> .decoder)
