module Page.Logout exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Http
import String

import DatingApi as Api exposing (User, Credentials)
import Session exposing (Session, Details)
import Routing exposing (Route(..))
import UI.Elements as El


-- MODEL


type alias Model =
    { session : Session
    , title : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session "Logout"
    , Session.logout
    )


-- UPDATE

type Msg
    = HandleLogout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleLogout ->
            ( model, Session.logout )


sendLogout : (Result Http.Error User -> msg) -> Credentials -> Cmd msg
sendLogout responseMsg creds =
    Http.send responseMsg (Api.postLogin creds)


responseToString : Maybe String -> String
responseToString r =
    case r of
        Just msg ->
            msg

        Nothing ->
            ""


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        El.contentWithHeader "Log out"
            []
    }


-- VALIDATION

