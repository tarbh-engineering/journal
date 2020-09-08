port module Ports exposing (boot, buy, clearAuth, log, onUrlChange, paymentFail, pushUrl, saveAuth)

import Json.Decode exposing (Value)
import Types



-- Out


port buy : { annual : Bool, email : String } -> Cmd msg


port clearAuth : () -> Cmd msg


port saveAuth : Value -> Cmd msg


port log : String -> Cmd msg


port pushUrl : String -> Cmd msg



-- In


port paymentFail : (() -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg


port boot : (Types.BootFlags -> msg) -> Sub msg
