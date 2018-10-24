module Page.Login exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Events as Events
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Http
import String

import DatingApi as Api exposing (User, Credentials)
import Session exposing (Session, Details)
import Routing exposing (Route(..))
import UI.Elements as El
import UI.Styles exposing (formInputStyle, centeredFillStyle, acceptButtonStyle)


-- MODEL


type alias Model =
    { session : Session
    , title : String
    , username : String
    , password : String
    , response : Maybe String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , title = "Login"
      , username = "Bargsteen"
      , password = "repsak"
      , response = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = EntryChanged Model
    | LoginClicked
    | HandleUserLogin (Result Http.Error User)
    | SessionChanged Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EntryChanged changedModel ->
            ( changedModel, Cmd.none )

        LoginClicked ->
            ( model, sendLogin (Credentials model.username model.password) )

        HandleUserLogin result ->
            case result of
                Ok user ->
                    ( model, Session.login user )

                Err errResponse ->
                    ( handleErrorResponse model errResponse, Cmd.none )

        SessionChanged session ->
            case session of
                Session.Guest key ->
                    ( { model | session = session }
                    , Routing.replaceUrl key (Routing.routeToString Home)
                    )
                Session.LoggedIn key _ ->
                    ( { model | session = session }
                    , Routing.replaceUrl key (Routing.routeToString ListUsers)
                    )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Session.onChange SessionChanged (Session.getNavKey model.session)


-- VIEW


view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        El.pageContent "Sign in" <|
            [ El.formColumn <|
                [  Input.username
                    (formInputStyle Element.none)
                    { text = model.username
                    , placeholder = El.placeholder "Username"
                    , onChange = \new -> EntryChanged { model | username = new }
                    , label = El.formLabel "Username"
                    }
                , Input.currentPassword
                    (formInputStyle Element.none)
                    { text = model.password
                    , placeholder = El.placeholder "Password"
                    , onChange = \new -> EntryChanged { model | password = new }
                    , label = El.formLabel "Password"
                    , show = False
                    }
                , El.messageButton (centeredFillStyle ++ acceptButtonStyle) LoginClicked "Sign in"
                , Element.text (responseToString model.response)
                ]
            ]
    }



handleErrorResponse : Model -> Http.Error -> Model
handleErrorResponse model errResponse =
    case errResponse of
        Http.BadUrl url ->
            { model | response = Just <| "Bad url: " ++ url }

        Http.BadPayload _ _ ->
            { model | response = Just "Bad payload" }

        Http.Timeout ->
            { model | response = Just "Timeout" }

        Http.NetworkError ->
            { model | response = Just "Networkerror" }

        Http.BadStatus statusResponse ->
            { model | response = Just <| "Badstatus" ++ .body statusResponse }



sendLogin : Credentials -> Cmd Msg
sendLogin creds =
    Http.send HandleUserLogin (Api.postLogin creds)


responseToString : Maybe String -> String
responseToString r =
    case r of
        Just msg ->
            msg

        Nothing ->
            ""
