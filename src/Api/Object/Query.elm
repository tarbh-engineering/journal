-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Query exposing (..)

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


type alias CheckRequiredArguments =
    { ciph : String
    , iv : String
    }


check :
    CheckRequiredArguments
    -> SelectionSet Bool Api.Object.Query
check requiredArgs =
    Object.selectionForField "Bool" "check" [ Argument.required "ciph" requiredArgs.ciph Encode.string, Argument.required "iv" requiredArgs.iv Encode.string ] Decode.bool


type alias NonceRequiredArguments =
    { email : String }


nonce :
    NonceRequiredArguments
    -> SelectionSet String Api.Object.Query
nonce requiredArgs =
    Object.selectionForField "String" "nonce" [ Argument.required "email" requiredArgs.email Encode.string ] Decode.string


refresh : SelectionSet (Maybe CustomScalars.Jwt) Api.Object.Query
refresh =
    Object.selectionForField "(Maybe CustomScalars.Jwt)" "refresh" [] (CustomScalars.codecs |> Api.Scalar.unwrapCodecs |> .codecJwt |> .decoder |> Decode.nullable)
