import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.CreateUser as CreateUser
import Page.ListUsers as ListUsers
import Page.Messages as Messages
import Url
import Routing as Routing exposing (Route(..), fromUrl, replaceUrl)
import Skeleton
import Debug



-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }


-- MODEL


type alias Model =
  { key : Nav.Key
  , page : Page
  }

type Page
  = CreateUser CreateUser.Model
  | ListUsers ListUsers.Model
  | Messages Messages.Model


init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  (Model key (CreateUser (CreateUser.initialModel)), Cmd.none)

-- UPDATE

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | CreateUserMsg CreateUser.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          Debug.log "internal" (model, Nav.pushUrl model.key (Url.toString url))
        Browser.External href ->
          Debug.log "external" (model, Nav.load href)
    UrlChanged url ->
      case Routing.fromUrl url of
        Just Routing.CreateUser -> 
          Debug.log "createuser" ({model | page = CreateUser CreateUser.initialModel}, Cmd.none)
        Just Routing.ListUsers ->
          Debug.log "listUsers" ({model | page = ListUsers ListUsers.initialModel}, Cmd.none)
        Just Routing.Messages -> 
          Debug.log "messages" ({model | page = Messages Messages.initialModel}, Cmd.none)
        Just x -> Debug.log ("Url Changed Just:" ++ Debug.toString x) (model, Cmd.none)
        Nothing -> Debug.log "Url Change Nothing" (model, Cmd.none)
    _ -> (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

view : Model -> Browser.Document Msg
view model =
  case model.page of
    CreateUser createUser ->
      Skeleton.view never (CreateUser.view createUser)
    ListUsers listUsers ->
      Skeleton.view never (ListUsers.view listUsers)
    Messages unused ->
      Skeleton.view never (Messages.view unused)
