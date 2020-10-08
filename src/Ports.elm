port module Ports exposing (buy, clearState, log, onUrlChange, paymentFail, pushUrl, saveState)

import Json.Decode exposing (Value)



-- Out


port buy : String -> Cmd msg


port clearState : () -> Cmd msg


port saveState : Value -> Cmd msg


port log : String -> Cmd msg


port pushUrl : String -> Cmd msg



-- In


port paymentFail : (() -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg
