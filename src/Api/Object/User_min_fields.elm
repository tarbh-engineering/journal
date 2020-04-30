-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.User_min_fields exposing (..)

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


email : SelectionSet (Maybe String) Api.Object.User_min_fields
email =
    Object.selectionForField "(Maybe String)" "email" [] (Decode.string |> Decode.nullable)


id : SelectionSet (Maybe CustomScalars.Uuid) Api.Object.User_min_fields
id =
    Object.selectionForField "(Maybe CustomScalars.Uuid)" "id" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecUuid |> .decoder |> Decode.nullable)


nonce : SelectionSet (Maybe String) Api.Object.User_min_fields
nonce =
    Object.selectionForField "(Maybe String)" "nonce" [] (Decode.string |> Decode.nullable)


password : SelectionSet (Maybe String) Api.Object.User_min_fields
password =
    Object.selectionForField "(Maybe String)" "password" [] (Decode.string |> Decode.nullable)
