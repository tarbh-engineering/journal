-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Mutation exposing (..)

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


type alias LoginRequiredArguments =
    { email : String
    , password : String
    }


login : LoginRequiredArguments -> SelectionSet String Api.Object.Mutation
login requiredArgs =
    Object.selectionForField "String" "login" [ Argument.required "email" requiredArgs.email Encode.string, Argument.required "password" requiredArgs.password Encode.string ] Decode.string


type alias SignupRequiredArguments =
    { email : String
    , nonce : String
    , password : String
    }


signup : SignupRequiredArguments -> SelectionSet String Api.Object.Mutation
signup requiredArgs =
    Object.selectionForField "String" "signup" [ Argument.required "email" requiredArgs.email Encode.string, Argument.required "nonce" requiredArgs.nonce Encode.string, Argument.required "password" requiredArgs.password Encode.string ] Decode.string
