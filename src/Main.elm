module Main exposing (main)

import Browser
import Browser.Events
import Day
import Derberos.Date.Utils exposing (numberToMonth)
import Helpers.UuidDict as UD
import Ports
import Routing
import Time
import Types exposing (Flags, Model, Msg, Screen)
import Update exposing (update)
import View exposing (view)


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { emptyModel
        | screen = flags.screen
        , isMobile = flags.isMobile
        , month = numberToMonth flags.month |> Maybe.withDefault Time.Jan
        , year = flags.year
        , tall = flags.screen.height >= 660
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onVisibilityChange Types.VisibilityChange
        , Ports.paymentFail (always Types.PaymentFail)
        , Ports.boot Types.Boot
        , Ports.onUrlChange
            (Routing.router
                >> Types.UrlChange
            )
        , Browser.Events.onResize Screen
            |> Sub.map Types.Resize
        ]


emptyModel : Model
emptyModel =
    { errors = []
    , isMobile = False
    , posts = Day.newDayDict
    , tags = UD.empty
    , view = Types.ViewHome
    , postCreateTags = []
    , postEditorBody = ""
    , postBeingEdited = False
    , postView = False
    , tagView = False
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
        , tags = []
        }
    , thanks = False
    , status = Types.Waiting
    , swActive = False
    , faq = False
    , dropdown = False
    , tall = False
    , tagsView = Types.TagsView
    , tagsSort = Types.SortName
    , tagsSortReverse = False
    }
