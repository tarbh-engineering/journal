-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Purchase exposing (..)

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


checkout_session_id : SelectionSet String Api.Object.Purchase
checkout_session_id =
    Object.selectionForField "String" "checkout_session_id" [] Decode.string


created_at : SelectionSet CustomScalars.Timestamptz Api.Object.Purchase
created_at =
    Object.selectionForField "CustomScalars.Timestamptz" "created_at" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder)


email : SelectionSet String Api.Object.Purchase
email =
    Object.selectionForField "String" "email" [] Decode.string


id : SelectionSet CustomScalars.Uuid Api.Object.Purchase
id =
    Object.selectionForField "CustomScalars.Uuid" "id" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecUuid |> .decoder)


updated_at : SelectionSet CustomScalars.Timestamptz Api.Object.Purchase
updated_at =
    Object.selectionForField "CustomScalars.Timestamptz" "updated_at" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecTimestamptz |> .decoder)


{-| An object relationship
-}
user : SelectionSet decodesTo Api.Object.User -> SelectionSet (Maybe decodesTo) Api.Object.Purchase
user object_ =
    Object.selectionForCompositeField "user" [] object_ (identity >> Decode.nullable)
