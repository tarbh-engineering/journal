module Types exposing (App(..), Auth, Cipher, Def(..), Flags, Funnel(..), GqlResult, GqlTask, Keys, Model, Msg(..), Post, PostRaw, Route(..), Screen, Sort(..), Status(..), Tag, TagRaw, View(..))

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Events exposing (Visibility(..))
import CustomScalars exposing (Jwt, Uuid)
import Date exposing (Date)
import Day exposing (DayDict)
import Graphql.Http
import Helpers.UuidDict exposing (UuidDict)
import Json.Decode exposing (Value)
import Task exposing (Task)
import Time exposing (Month(..))


type Status a
    = Missing
    | Loading (Maybe a)
    | Found a


type alias Flags =
    { month : Int
    , year : Int
    , online : Bool
    , screen : Screen
    , isMobile : Bool
    , swEnabled : Bool
    }


type alias Auth =
    { key : Value
    , token : Jwt
    }


type alias Screen =
    { width : Int
    , height : Int
    }


type alias Model =
    { posts : DayDict (Status Post)
    , tags : UuidDict Tag
    , errors : List String
    , view : View
    , inProgress :
        { logout : Bool
        , login : Bool
        , post : Bool
        , tag : Bool
        , postDelete : Bool
        , monthlyPlan : Bool
        , annualPlan : Bool
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
    , loginForm : LoginForm
    , tagSort : Sort
    , online : Bool
    , searchString : String
    , selectedResult : Maybe Uuid
    , screen : Screen
    , isMobile : Bool
    , month : Month
    , year : Int
    , current : Maybe Date
    , funnel : Funnel
    , tag : Maybe Uuid
    , def : Maybe Def
    , magic : Maybe Bool
    , mg : ( String, String )
    , thanks : Bool
    , status : App
    , swEnabled : Bool
    , faq : Bool
    }


type App
    = Waiting
    | SwUnavailable
    | Ready


type Funnel
    = Hello
    | WelcomeBack String
    | JoinUs
    | CheckEmail


type Def
    = Alts
    | Secure
    | Private
    | Journal


type Sort
    = Alpha Bool


type alias LoginForm =
    { email : String
    , password : String
    , passwordVisible : Bool
    }


type alias Keys =
    { encryptionKey : Value
    , serverKey : String
    }


type Msg
    = PostsCb (GqlResult (List Post))
    | PostCb Date (GqlResult (Maybe Post))
    | PostMutateCb (GqlResult Post)
    | PostDeleteCb (GqlResult Date)
    | TagsCb (GqlResult (List Tag))
    | PostCancel
    | PostCreateSubmit Date
    | PostDelete Uuid Date
    | PostUpdateStart String
    | PostUpdateCancel
    | PostUpdateSubmit Uuid
    | BodyUpdate String
    | CheckCb (GqlResult Bool)
    | NonceCb (GqlResult String)
    | LoginCb (GqlResult Auth)
    | EmailSubmit
    | LoginSubmit String
    | SignupSubmit
    | Buy Bool
    | LoginFormEmailUpdate String
    | LoginFormPasswordUpdate String
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
    | TagSortUpdate Sort
    | PostTagToggle Post Tag
    | PostCreateTagToggle Date Tag
    | FocusCb (Result Browser.Dom.Error ())
    | UrlChange (Maybe Route)
    | UrlRequest UrlRequest
    | NavigateTo Route
    | SetOnline Bool
    | SetSelectedResult Uuid
    | PostViewToggle
    | TagViewToggle
    | VisibilityChange Visibility
    | Resize Screen
    | PrevMonth
    | NextMonth
    | FaqToggle
    | Force
    | Change
    | TagSelect Uuid
    | SetDef Def
    | RefreshCb (Auth -> Cmd Msg) (GqlResult (Maybe Jwt))
    | Bad (Auth -> Cmd Msg)
    | ExportPosts
    | EmailCb
    | Boot { key : Maybe String, href : String }


type Route
    = RouteToday
    | RouteCalendar
    | RouteDay Date
    | RouteHome
    | RouteTags
    | RouteSettings
    | RouteStats


type alias GqlResult a =
    Result (Graphql.Http.Error ()) a


type alias GqlTask a =
    Task (Graphql.Http.Error ()) a


type alias Post =
    { date : Date
    , body : String
    , id : Uuid
    , tags : List Uuid
    }


type alias PostRaw =
    { date : Date
    , cipher : Maybe Cipher
    , id : Uuid
    , tags : List Uuid
    }


type alias Cipher =
    { iv : String
    , ciphertext : String
    }


type alias Tag =
    { name : String
    , id : Uuid
    , count : Int
    }


type alias TagRaw =
    { cipher : Cipher
    , id : Uuid
    , count : Int
    }


type View
    = ViewHome
    | ViewCalendar
    | ViewSettings
    | ViewSuccess
    | ViewTags
    | ViewMagic
    | ViewStats
