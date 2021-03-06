module Types exposing (Auth, BootAction(..), Cipher, Def(..), EmailRes(..), Flags, Funnel(..), GqlResult, GqlTask, Keys, Model, Msg(..), Post, PostRaw, PostTag, PostTagRes, PostWithTagRes, Route(..), Screen, Tag, TagRaw, TagsSort(..), TagsView(..), View(..))

import Array exposing (Array)
import Browser.Dom
import Browser.Events exposing (Visibility)
import Calendar exposing (Date)
import CustomScalars exposing (Jwt, Uuid)
import DateTime exposing (DateTime)
import Day exposing (DayDict)
import Email exposing (Email)
import Graphql.Http
import Helpers.UuidDict exposing (UuidDict)
import Json.Decode exposing (Value)
import Task exposing (Task)
import Time exposing (Month, Weekday)


type alias Model =
    { posts : DayDict Post
    , tags : UuidDict Tag
    , view : View
    , inProgress :
        { logout : Bool
        , login : Bool
        , post : Bool
        , tag : Bool
        , postDelete : Bool
        , buy : Bool
        , tags : List Uuid
        , postTags : List ( Date, Uuid )
        }
    , postEditorBody : String
    , postBeingEdited : Bool
    , postView : Bool
    , tagView : Bool
    , postCreateTags : List Uuid
    , auth : Maybe Auth
    , tagCreateName : String
    , tagBeingEdited : Maybe Uuid
    , tagUpdate : String
    , flash : Maybe ( Array ( Bool, String ), List ( Int, String ) )
    , loginForm :
        { email : String
        , password : String
        , passwordConfirm : String
        , passwordVisible : Bool
        , err : Maybe String
        }
    , searchString : String
    , screen : Screen
    , isMobile : Bool
    , month : Month
    , year : Int
    , current : Maybe Date
    , funnel : Funnel
    , tag : Maybe Uuid
    , def : Maybe Def
    , magic : Maybe Bool
    , thanks : Bool
    , swActive : Bool
    , tagsView : TagsView
    , tagsSort : TagsSort
    , tagsSortReverse : Bool
    , postSortReverse : Bool
    , weekStart : Weekday
    , today : Date
    , area : Int
    , landscape : Bool
    , charge : Int
    , coinbase : String
    }


type Msg
    = PostsCb (GqlResult (List Post))
    | PostCb Date (GqlResult (Maybe Post))
    | PostTagCb ( Date, Uuid ) (GqlResult PostTagRes)
    | PostWithTagCb ( Date, Uuid ) (GqlResult PostWithTagRes)
    | PostMutateCb (GqlResult Post)
    | TagsCb (GqlResult (List Tag))
    | CellSelect Date
    | PostUpdateStart
    | PostUpdateCancel
    | PostBodySubmit
    | BodyUpdate String
    | CheckCb (GqlResult Bool)
    | NonceCb Email (GqlResult EmailRes)
    | LoginCb (GqlResult Auth)
    | EmailSubmit
    | LoginSubmit Email String
    | SignupSubmit (Keys -> String -> GqlTask Jwt)
    | Buy Email
    | PaymentFail
    | LoginFormEmailUpdate String
    | LoginFormPasswordUpdate String
    | LoginFormPasswordConfirmUpdate String
    | LoginFormPasswordVisibleToggle
    | Logout
    | LogoutCb (GqlResult Bool)
    | InitCb (Maybe Route) (GqlResult (Maybe Auth))
    | TagDelete Tag
    | TagDeleteCb (GqlResult Uuid)
    | TagUpdate String
    | TagUpdateSet (Maybe Tag)
    | TagUpdateSubmit Tag
    | TagUpdateCb (GqlResult Tag)
    | TagCreateNameUpdate String
    | TagCreateSubmit
    | TagCreateCb (GqlResult Tag)
    | FocusCb (Result Browser.Dom.Error ())
    | UrlChange (Result String Route)
    | NavigateTo Route
    | GoToToday (Maybe Date)
    | ReadyStart (Maybe Date)
    | VisibilityChange Visibility
    | Resize Screen
    | PrevMonth
    | NextMonth
    | FunnelCancel
    | TagSelect Uuid
    | TagDeselect
    | SetDef Def
    | RefreshCb (Auth -> Cmd Msg) (GqlResult (Maybe Jwt))
    | JwtFailure (Auth -> Cmd Msg)
    | TagsViewSet TagsView
    | TagsSortSet TagsSort
    | PostSortToggle
    | WeekdaySet Weekday
    | TodaySet Date
    | PostTagAttach Date Uuid
    | PostTagDetach Date Uuid
    | ServiceWorkerFail


type alias Flags =
    { month : Int
    , year : Int
    , screen : Screen
    , isMobile : Bool
    , href : String
    , key : Maybe String
    , swActive : Bool
    , charge : Int
    , coinbase : String
    }


type BootAction
    = BootSignup String
    | BootPaymentFail
    | BootPaymentSuccess


type Route
    = RouteCalendar
    | RouteDay Date
    | RouteDayDetail Date
    | RouteDayTags Date
    | RouteHome
    | RouteTags
    | RouteTag
    | RouteSettings


type View
    = ViewHome
    | ViewCalendar
    | ViewSettings
    | ViewTags


type alias GqlResult a =
    Result (Graphql.Http.Error ()) a


type alias GqlTask a =
    Task (Graphql.Http.Error ()) a


type alias Auth =
    { key : Value
    , token : Jwt
    }


type alias Screen =
    { width : Int
    , height : Int
    }


type TagsSort
    = SortName
    | SortDate
    | SortUsage


type TagsView
    = TagsView
    | TagsCreate
    | TagsSort


type Funnel
    = Hello
    | WelcomeBack Email String
    | JoinUs Email
    | GuestSignup Email
    | Signup String
    | PayErr
    | PayOk
    | SwFail


type Def
    = Private
    | Devices
    | OpenSource
    | Control


type EmailRes
    = Guest String
    | Nonce String
    | Newbie


type alias Keys =
    { encryptionKey : Value
    , serverKey : String
    }


type alias Post =
    { date : Date
    , body : Maybe String
    , id : Uuid
    , tags : List PostTag
    }


type alias PostTag =
    { id : Uuid
    , tag : Uuid
    }


type alias PostTagRes =
    { postDate : Date
    , postTags : List PostTag
    , tagId : Uuid
    , tagPosts : List Date
    }


type alias PostWithTagRes =
    { post : Post
    , tagId : Uuid
    , tagPosts : List Date
    }


type alias PostRaw =
    { date : Date
    , cipher : Maybe Cipher
    , id : Uuid
    , tags : List PostTag
    }


type alias Cipher =
    { iv : String
    , ciphertext : String
    }


type alias Tag =
    { name : String
    , id : Uuid
    , posts : List Date
    , created : DateTime
    }


type alias TagRaw =
    { cipher : Cipher
    , id : Uuid
    , posts : List Date
    , created : DateTime
    }
