module Types exposing (Auth, BootFlags, Cipher, Def(..), Flags, Funnel(..), GqlResult, GqlTask, Keys, Model, Msg(..), Post, PostRaw, PostTag, Route(..), Screen, Tag, TagRaw, TagsSort(..), TagsView(..), View(..))

import Array exposing (Array)
import Browser.Dom
import Browser.Events exposing (Visibility)
import Calendar exposing (Date)
import CustomScalars exposing (Jwt, Uuid)
import DateTime exposing (DateTime)
import Day exposing (DayDict)
import Graphql.Http
import Helpers.UuidDict exposing (UuidDict)
import Json.Decode exposing (Value)
import Task exposing (Task)
import Time exposing (Month, Weekday)


type alias Model =
    { posts : DayDict Post
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
    , loginForm : LoginForm
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
    , faq : Bool
    , dropdown : Bool
    , tall : Bool
    , tagsView : TagsView
    , tagsSort : TagsSort
    , tagsSortReverse : Bool
    , postSortReverse : Bool
    , weekStart : Weekday
    , today : Date
    , area : Int
    , landscape : Bool
    }


type Msg
    = PostsCb (GqlResult (List Post))
    | PostCb Date (GqlResult (Maybe Post))
    | PostTagCb ( Date, Uuid ) (GqlResult Post)
    | PostMutateCb (GqlResult Post)
    | PostDeleteCb (GqlResult Date)
    | TagsCb (GqlResult (List Tag))
    | PostViewCancel
    | PostCreateSubmit
    | PostDelete Uuid Date
    | PostUpdateStart
    | PostUpdateCancel
    | PostUpdateSubmit Uuid
    | BodyUpdate String
    | CheckCb (GqlResult Bool)
    | NonceCb (GqlResult String)
    | LoginCb (GqlResult Auth)
    | EmailSubmit
    | LoginSubmit String
    | SignupSubmit String
    | Buy Bool
    | PaymentFail
    | LoginFormEmailUpdate String
    | LoginFormPasswordUpdate String
    | LoginFormPasswordVisibleToggle
    | Logout
    | LogoutCb (GqlResult Bool)
    | InitCb (Maybe Route) (GqlResult (Maybe Auth))
    | PostViewTagStart
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
    | GoToToday
    | PostClear Post
    | PostViewStart
    | TagViewToggle
    | VisibilityChange Visibility
    | Resize Screen
    | PrevMonth
    | NextMonth
    | FaqToggle
    | Change
    | TagSelect Uuid
    | TagDeselect
    | SetDef Def
    | RefreshCb (Auth -> Cmd Msg) (GqlResult (Maybe Jwt))
    | Bad (Auth -> Cmd Msg)
    | Boot BootFlags
    | DropdownToggle
    | TagsViewSet TagsView
    | TagsSortSet TagsSort
    | PostSortToggle
    | WeekdaySet Weekday
    | TodaySet Date
    | PostTagAttach Date Uuid
    | PostTagDetach Date Uuid


type alias Flags =
    { month : Int
    , year : Int
    , screen : Screen
    , isMobile : Bool
    }


type alias BootFlags =
    { key : Maybe String
    , href : String
    , swActive : Bool
    }


type Route
    = RouteToday
    | RouteCalendar
    | RouteDay Date
    | RouteDayDetail Date
    | RouteHome
    | RouteTags
    | RouteTag
    | RouteSettings


type View
    = ViewHome
    | ViewCalendar
    | ViewSettings
    | ViewSuccess
    | ViewTags
    | ViewSignup String


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
    | WelcomeBack String
    | JoinUs
    | CheckEmail


type Def
    = Secure
    | Private
    | Journal


type alias LoginForm =
    { email : String
    , password : String
    , passwordVisible : Bool
    }


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
