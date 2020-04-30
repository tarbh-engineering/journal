-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Tag_aggregate exposing (..)

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


aggregate : SelectionSet decodesTo Api.Object.Tag_aggregate_fields -> SelectionSet (Maybe decodesTo) Api.Object.Tag_aggregate
aggregate object_ =
    Object.selectionForCompositeField "aggregate" [] object_ (identity >> Decode.nullable)


nodes : SelectionSet decodesTo Api.Object.Tag -> SelectionSet (List decodesTo) Api.Object.Tag_aggregate
nodes object_ =
    Object.selectionForCompositeField "nodes" [] object_ (identity >> Decode.list)
