module Types exposing (Auth, Cipher, Def(..), Flags, Funnel(..), GqlResult, GqlTask, Keys, Model, Msg(..), Post, PostRaw, PostView(..), Route(..), Screen, Sort(..), Status(..), Tag, TagRaw, View(..))

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Events exposing (Visibility(..))
import Date exposing (Date)
import Day exposing (DayDict)
import Graphql.Http
import Helpers.UuidDict exposing (UuidDict)
import Json.Decode exposing (Value)
import Task exposing (Task)
import Time exposing (Month(..))
import Uuid exposing (Uuid)


type Status a
    = Missing
    | Loading (Maybe a)
    | Found a


type alias Flags =
    { auth : Maybe Auth
    , month : Int
    , year : Int
    , online : Bool
    , href : String
    , screen : Screen
    }


type alias Auth =
    { key : Value
    , token : String
    , email : String
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
    , postEditorBody : String
    , postSaveInProgress : Bool
    , postBeingEdited : Bool
    , postView : PostView
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
    , month : Month
    , year : Int
    , current : Maybe Date
    , funnel : Funnel
    , tag : Maybe Uuid
    , def : Maybe Def
    , magic : Maybe Bool
    , mg : String
    }


type Funnel
    = Hello
    | WelcomeBack String
    | JoinUs
    | CheckEmail


type Def
    = World
    | First
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
    | PostCreateSubmit Date
    | PostDelete Uuid
    | PostUpdateStart String
    | PostUpdateCancel
    | PostUpdateSubmit Uuid
    | BodyUpdate String
    | CheckCb (GqlResult Bool)
    | NonceCb (GqlResult String)
    | AuthCb (GqlResult Auth)
    | EmailSubmit
    | LoginSubmit String
    | SignupSubmit String
    | Buy Bool
    | LoginFormEmailUpdate String
    | LoginFormPasswordUpdate String
    | LoginFormPasswordVisibleToggle
    | Logout
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
    | PostCreateTagToggle Tag
    | FocusCb (Result Browser.Dom.Error ())
    | UrlChange Route
    | UrlRequest UrlRequest
    | NavigateTo Route
    | SetOnline Bool
    | SetSelectedResult Uuid
    | PostViewSet PostView
    | VisibilityChange Visibility
    | Resize Screen
    | PrevMonth
    | NextMonth
    | Force
    | Change
    | TagSelect Uuid
    | SetDef Def


type Route
    = NotFound
    | RouteToday
    | RouteCalendar
    | RouteDay Date
    | RouteHome
    | RouteTags
    | RouteSettings


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
    , cipher : Cipher
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


type PostView
    = PostView
    | PostTags
