port module Ports exposing (buy, clearAuth, log, onUrlChange, online, pushUrl, saveAuth)

import Json.Decode exposing (Value)


port buy : { annual : Bool, email : String } -> Cmd msg


port clearAuth : () -> Cmd msg


port saveAuth : Value -> Cmd msg


port log : String -> Cmd msg


port online : (Bool -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg


port pushUrl : String -> Cmd msg
