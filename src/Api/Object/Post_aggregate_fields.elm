-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Api.Object.Post_aggregate_fields exposing (..)

import Api.Enum.Post_select_column
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


type alias CountOptionalArguments =
    { columns : OptionalArgument (List Api.Enum.Post_select_column.Post_select_column)
    , distinct : OptionalArgument Bool
    }


count : (CountOptionalArguments -> CountOptionalArguments) -> SelectionSet (Maybe Int) Api.Object.Post_aggregate_fields
count fillInOptionals =
    let
        filledInOptionals =
            fillInOptionals { columns = Absent, distinct = Absent }

        optionalArgs =
            [ Argument.optional "columns" filledInOptionals.columns (Encode.enum Api.Enum.Post_select_column.toString |> Encode.list), Argument.optional "distinct" filledInOptionals.distinct Encode.bool ]
                |> List.filterMap identity
    in
    Object.selectionForField "(Maybe Int)" "count" optionalArgs (Decode.int |> Decode.nullable)


max : SelectionSet decodesTo Api.Object.Post_max_fields -> SelectionSet (Maybe decodesTo) Api.Object.Post_aggregate_fields
max object_ =
    Object.selectionForCompositeField "max" [] object_ (identity >> Decode.nullable)


min : SelectionSet decodesTo Api.Object.Post_min_fields -> SelectionSet (Maybe decodesTo) Api.Object.Post_aggregate_fields
min object_ =
    Object.selectionForCompositeField "min" [] object_ (identity >> Decode.nullable)
