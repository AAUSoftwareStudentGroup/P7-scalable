module Page.Logout exposing (Model, Msg(..), init, update, view)

import Api.Authentication exposing (Credentials)
import Api.Users exposing (User)
import Browser
import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Http
import Routing exposing (Route(..))
import Session exposing (Details, PageType(..), Session)
import String
import UI.Elements as El



-- MODEL


type alias Model =
    { session : Session
    , title : String
    , numTries : Int
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session "Logout" 0
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
                    if model.numTries > 10 then
                        ( model, Session.logout )

                    else
                        ( { model | numTries = model.numTries + 1 }, sendLogout HandleLogout model.session )


sendLogout : (Result Http.Error String.String -> msg) -> Session -> Cmd msg
sendLogout responseMsg session =
    case session of
        Session.LoggedIn _ _ _ userInfo ->
            Http.send responseMsg (Api.Users.postLogout userInfo)

        Session.Guest _ _ _ ->
            Cmd.none


responseToString : Maybe String -> String
responseToString r =
    case r of
        Just msg ->
            msg

        Nothing ->
            ""



-- VIEW


view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        Scrollable <|
            El.titledContent "Log out"
                []
    }
