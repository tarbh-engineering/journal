port module Ports exposing (boot, buy, clearState, log, onUrlChange, paymentFail, pushUrl, saveState)

import Json.Decode exposing (Value)
import Types



-- Out


port buy : { annual : Bool, email : String } -> Cmd msg


port clearState : () -> Cmd msg


port saveState : Value -> Cmd msg


port log : String -> Cmd msg


port pushUrl : String -> Cmd msg



-- In


port paymentFail : (() -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg


port boot : (Types.BootFlags -> msg) -> Sub msg
