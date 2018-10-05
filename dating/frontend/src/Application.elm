import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.CreateUser as CreateUser
import Url
import Skeleton



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
          (model, Nav.load (Url.toString url))
        Browser.External href ->
          (model, Nav.load href)

    UrlChanged url ->
      ( model
      , Cmd.none
      )
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
