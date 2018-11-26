module Page.Login exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Validate exposing (Validator, Valid)
import Http
import String


import Api.Authentication exposing (UserInfo, Credentials)
import Api.Users exposing (User)
import Session exposing (Session, PageType(..), Details)
import Common as Common
import Routing exposing (Route(..))
import UI.Elements as El


-- MODEL


type alias Model =
    { session   : Session
    , title     : String
    , errors    : List (Error)
    , attemptedSubmission: Bool
    , username  : String
    , password  : String
    , response  : Maybe String
    }

type alias Error =
    ( FormField, String )

type FormField
    = Username
    | Password


init : Session -> ( Model, Cmd Msg )
init session =
    ( updateErrors
        { session = session
        , title = "Login"
        , errors = []
        , attemptedSubmission = False
        , username = ""
        , password = ""
        , response = Nothing
        }
    , Cmd.none
    )


-- UPDATE


type Msg
    = FormFieldChanged FormField String
    | Submitted
    | HandleUserLogin (Result Http.Error UserInfo)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormFieldChanged field value ->
            ( updateErrors <| setField model field value
            , Cmd.none
            )

        Submitted ->
            case Validate.validate modelValidator model of
                Ok validForm ->
                    ( { model | errors = [] }
                    , sendLogin HandleUserLogin (credsFromValidForm validForm)
                    )
                Err errors ->
                    ( { model | errors = errors, attemptedSubmission = True }
                    , Cmd.none
                    )

        HandleUserLogin result ->
            case result of
                Ok userInfo ->
                    ( model, Session.login userInfo )

                Err errResponse ->
                    case errResponse of
                        Http.BadUrl url ->
                            ( { model | session = Session.addNotification model.session ("Bad url: " ++ url) }, Cmd.none )

                        Http.BadPayload _ _ ->
                            ( { model | session = Session.addNotification model.session "Invalid data sent to server" }, Cmd.none )

                        Http.Timeout ->
                            ( { model | session = Session.addNotification model.session "Couldn't reach server" }, Cmd.none )

                        Http.NetworkError ->
                            ( { model | session = Session.addNotification model.session "Couldn't reach server" }, Cmd.none )

                        Http.BadStatus statusResponse ->
                            ( { model | session = Session.addNotification model.session ("Error: " ++ .body statusResponse) }, Cmd.none )


setField : Model -> FormField -> String -> Model
setField model field value =
    case field of
        Username ->
            { model | username = value }
        Password ->
            { model | password = value }


updateErrors : Model -> Model
updateErrors model =
    case Validate.validate modelValidator model of
        Ok validForm ->
            { model | errors = [] }
        Err errors ->
            { model | errors = errors }


credsFromValidForm : Valid Model -> Credentials
credsFromValidForm validForm =
    let
        model = Validate.fromValid validForm
    in
        Credentials model.username model.password


sendLogin : (Result Http.Error UserInfo -> msg) -> Credentials -> Cmd msg
sendLogin responseMsg creds =
    Http.send responseMsg (Api.Users.postLogin creds)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids = Scrollable
        <| El.titledContent "Sign in"
            [ Html.form [ classList
                            [ ( "grid", True )
                            , ( "l-12", True )
                            , ( "s-12", True )
                            ]
                        --, Events.onSubmit (FuckNotifications "Test")
                        , Events.onSubmit Submitted
                        ]
                [ El.validatedInput Username "text" "Username" model.username FormFieldChanged True model.errors model.attemptedSubmission
                , El.validatedInput Password "password" "Password" model.password FormFieldChanged True model.errors model.attemptedSubmission
                , El.submitButton "Sign in"
                ]
            ]
    }


-- VALIDATION


modelValidator : Validator ( FormField, String ) Model
modelValidator =
    Validate.all
        [ Validate.ifBlank .username ( Username, "Please enter a username" )
        , Validate.ifBlank .password ( Password, "Please enter a password" )
        ]
