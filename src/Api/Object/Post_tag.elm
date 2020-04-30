-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Post_tag exposing (..)

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


id : SelectionSet CustomScalars.Uuid Api.Object.Post_tag
id =
    Object.selectionForField "CustomScalars.Uuid" "id" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecUuid |> .decoder)


{-| An object relationship
-}
postBypostId : SelectionSet decodesTo Api.Object.Post -> SelectionSet decodesTo Api.Object.Post_tag
postBypostId object_ =
    Object.selectionForCompositeField "postBypostId" [] object_ identity


post_id : SelectionSet CustomScalars.Uuid Api.Object.Post_tag
post_id =
    Object.selectionForField "CustomScalars.Uuid" "post_id" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecUuid |> .decoder)


{-| An object relationship
-}
tagBytagId : SelectionSet decodesTo Api.Object.Tag -> SelectionSet decodesTo Api.Object.Post_tag
tagBytagId object_ =
    Object.selectionForCompositeField "tagBytagId" [] object_ identity


tag_id : SelectionSet CustomScalars.Uuid Api.Object.Post_tag
tag_id =
    Object.selectionForField "CustomScalars.Uuid" "tag_id" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecUuid |> .decoder)


{-| An object relationship
-}
userByuserId : SelectionSet decodesTo Api.Object.User -> SelectionSet decodesTo Api.Object.Post_tag
userByuserId object_ =
    Object.selectionForCompositeField "userByuserId" [] object_ identity


user_id : SelectionSet CustomScalars.Uuid Api.Object.Post_tag
user_id =
    Object.selectionForField "CustomScalars.Uuid" "user_id" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecUuid |> .decoder)
