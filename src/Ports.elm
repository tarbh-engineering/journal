port module Ports exposing (clearAuth, log, onUrlChange, online, pushUrl, saveAuth)

import Types exposing (Auth)


port clearAuth : () -> Cmd msg


port saveAuth : Auth -> Cmd msg


port log : String -> Cmd msg


port online : (Bool -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg


port pushUrl : String -> Cmd msg
