module Page.Logout exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Http
import String

import Api.Users exposing (User)
import Api.Authentication exposing (Credentials)
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
    , sendLogout HandleLogout session
    )


-- UPDATE

type Msg
    = HandleLogout (Result Http.Error String.String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleLogout result ->
            case result of
                Ok _ ->
                    ( model, Session.logout )
                Err _ ->
                    ( model, sendLogout HandleLogout model.session)


sendLogout : (Result Http.Error String.String -> msg) -> Session -> Cmd msg
sendLogout responseMsg session =
    Http.send responseMsg (Api.Users.postLogout <| Session.getUserToken session)


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

