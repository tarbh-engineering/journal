module Main exposing (main)

import Browser
import Browser.Events
import Day
import Derberos.Date.Utils exposing (numberToMonth)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Helpers.UuidDict as UD
import Ports
import Routing
import Time exposing (Month(..))
import Types exposing (Flags, Model, Msg(..), Route(..), Screen, Sort(..), Status(..), View(..))
import Update exposing (update)
import Url
import View exposing (view)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions =
            \_ ->
                Sub.batch
                    [ Browser.Events.onVisibilityChange VisibilityChange
                    , Ports.online SetOnline
                    , Ports.boot Boot
                    , Ports.onUrlChange
                        (Url.fromString
                            >> Maybe.andThen Routing.router
                            >> Types.UrlChange
                        )
                    , Browser.Events.onResize Screen
                        |> Sub.map Resize
                    ]
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { emptyModel
        | screen = flags.screen
        , isMobile = flags.isMobile
        , swEnabled = flags.swEnabled
        , month = numberToMonth flags.month |> Maybe.withDefault Time.Jan
        , year = flags.year
      }
    , Cmd.none
    )


emptyModel : Model
emptyModel =
    { errors = []
    , isMobile = False
    , posts = Day.newDayDict
    , tags = UD.empty
    , view = ViewHome
    , postCreateTags = []
    , postEditorBody = ""
    , postBeingEdited = False
    , postView = False
    , auth = Nothing
    , tagCreateName = ""
    , tagUpdate = ""
    , tagBeingEdited = Nothing
    , flash = Nothing
    , loginForm =
        { password = ""
        , email = ""
        , passwordVisible = False
        }
    , tagSort = Alpha False
    , online = True
    , searchString = ""
    , selectedResult = Nothing
    , screen = { height = 0, width = 0 }
    , month = Time.Jan
    , year = 2020
    , current = Nothing
    , funnel = Types.Hello
    , tag = Nothing
    , def = Nothing
    , magic = Nothing
    , mg = ( "", "" )
    , inProgress =
        { logout = False
        , login = False
        , post = False
        , tag = False
        , postDelete = False
        , monthlyPlan = False
        , annualPlan = False
        }
    , thanks = False
    , status = Types.Waiting
    , swEnabled = False
    , faq = False
    }
