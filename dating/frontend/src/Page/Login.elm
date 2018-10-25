module Page.Login exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (Html, div)
import Html.Attributes as Attributes
import Html.Events as Events
import Validate exposing (Validator, Valid)
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
    , errors : List (Error)
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
    ( { session = session
      , title = "Login"
      , errors = []
      , username = "Bargsteen"
      , password = "repsak"
      , response = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = FormFieldChanged FormField String
    | Submitted
    | HandleUserLogin (Result Http.Error User)
    | SessionChanged Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormFieldChanged field value ->
            ( setField model field value, Cmd.none )

        Submitted ->
            case Validate.validate modelValidator model of
                Ok validForm ->
                    ( { model | errors = [] }
                    , sendLogin HandleUserLogin (credsFromValidForm validForm)
                    )
                Err errors ->
                    ( { model | errors = errors }
                    , Cmd.none
                    )

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

setField : Model -> FormField -> String -> Model
setField model field value =
    case field of
        Username ->
            { model | username = value }
        Password ->
            { model | password = value }


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
        El.pageContent "Sign in"
            [ Html.form [ Events.onSubmit Submitted ]
                [ El.validatedInput Username "text" "Username" "Username" model.username FormFieldChanged model.errors
                , El.validatedInput Password "password" "Password" "Password" model.password FormFieldChanged model.errors
                , El.submitButton "Sign in"
                ]
            , Html.text (responseToString model.response)
            ]
    }


-- VALIDATION

modelValidator : Validator ( FormField, String ) Model
modelValidator =
    Validate.all
        [ Validate.ifBlank .username ( Username, "Username can't be blank." )
        , Validate.ifBlank .password ( Password, "Password can't be blank." )
        ]

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



credsFromValidForm : Valid Model -> Credentials
credsFromValidForm validForm =
    let
        model = Validate.fromValid validForm
    in
        Credentials model.username model.password


sendLogin : (Result Http.Error User -> msg) -> Credentials -> Cmd msg
sendLogin responseMsg creds =
    Http.send responseMsg (Api.postLogin creds)


responseToString : Maybe String -> String
responseToString r =
    case r of
        Just msg ->
            msg

        Nothing ->
            ""
