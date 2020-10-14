module Email exposing (Email, parse, toString)

import Validate exposing (isValidEmail)


type Email
    = Email String


toString : Email -> String
toString (Email str) =
    str


parse : String -> Maybe Email
parse =
    String.toLower
        >> String.trim
        >> (\str ->
                if isValidEmail str then
                    Email str
                        |> Just

                else
                    Nothing
           )
