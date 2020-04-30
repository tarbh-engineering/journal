module Types exposing (Auth, Emoji(..), Flags, Funnel(..), GqlResult, GqlTask, Keys, Model, Msg(..), Post, PostView(..), Route(..), Screen, ServiceWorkerRequest(..), Sort(..), Status(..), Tag, View(..))

import Array exposing (Array)
import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Events exposing (Visibility(..))
import Date exposing (Date)
import Day exposing (DayDict)
import Dict exposing (Dict)
import Graphql.Http
import Helpers.UuidDict exposing (UuidDict)
import Json.Decode exposing (Value)
import List.Nonempty exposing (Nonempty)
import Set exposing (Set)
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
    , tagsBeingEdited : UuidDict String
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
    , force : Bool
    , funnel : Funnel
    , tag : Maybe Uuid
    }


type Funnel
    = Hello
    | WelcomeBack String
    | JoinUs


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
    | NonceCb (GqlResult (Maybe String))
    | AuthCb (GqlResult Auth)
    | Login String String
    | EmailSubmit
    | LoginSubmit String
    | SignupSubmit
    | LoginFormEmailUpdate String
    | LoginFormPasswordUpdate String
    | LoginFormPasswordVisibleToggle
    | Logout
    | Signup String String
    | TagDelete Tag
    | TagDeleteCb (GqlResult Uuid)
    | TagUpdate Tag (Maybe String)
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
    | Demo


type Route
    = NotFound
    | RouteToday
    | RouteDay Date
    | RouteWeek Date
    | RouteMonth Month Int
    | RouteYear Int
    | RouteHome
    | RouteTags
    | RouteSettings
    | RouteLogin
    | RouteTagPosts Uuid


type alias GqlResult a =
    Result (Graphql.Http.Error ()) a


type alias GqlTask a =
    Task (Graphql.Http.Error ()) a


type ServiceWorkerRequest
    = GenerateKeys
        { password : String
        , nonce : String
        }
    | GenerateNonce
    | Decrypt { key : Value, content : String }
    | Encrypt { key : Value, content : String }


type alias Post =
    { date : Date
    , body : String
    , id : Uuid
    , tags : List Uuid
    }


type alias Tag =
    { name : String
    , id : Uuid
    , count : Int
    }


type View
    = ViewPost Date
    | ViewLogin
    | ViewMonth Int Month
    | ViewWeek (Nonempty Date)
    | ViewTagPosts Uuid
    | ViewYear Int
    | ViewHome
    | ViewSettings
    | ViewTags


type PostView
    = PostView
    | PostTags


type Emoji
    = New
    | House
    | Edit
    | Trash
    | TagEmoji
    | Tick
    | X
    | Next
    | Disk
    | Previous
    | Key
    | Sun
    | Chart
    | Calendar
    | Dice
    | Cogs
    | Bang
    | Clock
    | Cyclone
    | Abc
    | Sherlock
    | LeftArrow
    | Paper
    | Locked
    | Unlocked
