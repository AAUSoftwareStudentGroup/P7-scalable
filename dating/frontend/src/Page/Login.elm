module Page.Login exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Validate exposing (Validator, Valid)
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
    , errors : List (Error)
    , attemptedSubmission: Bool
    , username : String
    , password : String
    , response : Maybe String
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
    | HandleUserLogin (Result Http.Error User)


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
                Ok user ->
                    ( model, Session.login user )

                Err errResponse ->
                    ( handleErrorResponse model errResponse, Cmd.none )


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


sendLogin : (Result Http.Error User -> msg) -> Credentials -> Cmd msg
sendLogin responseMsg creds =
    Http.send responseMsg (Api.postLogin creds)


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
        El.contentWithHeader "Sign in"
            [ Html.form [ classList
                            [ ( "grid", True )
                            , ( "l-12", True )
                            , ( "s-12", True )
                            ]
                        , Events.onSubmit Submitted
                        ]
                [ El.validatedInput Username "text" "Username" model.username FormFieldChanged model.errors model.attemptedSubmission
                , El.validatedInput Password "password" "Password" model.password FormFieldChanged model.errors model.attemptedSubmission
                , El.submitButton "Sign in"
                ]
            , Html.text (responseToString model.response)
            ]
    }


-- VALIDATION


modelValidator : Validator ( FormField, String ) Model
modelValidator =
    Validate.all
        [ Validate.ifBlank .username ( Username, "Please enter a username" )
        , Validate.ifBlank .password ( Password, "Please enter a password" )
        ]
