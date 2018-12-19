module Page.Login exposing (Model, Msg(..), init, update, view)

import Api.Authentication exposing (Credentials, UserInfo)
import Api.Users exposing (User)
import Browser
import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Http
import Routing exposing (Route(..))
import Session exposing (Details, Notification, PageType(..), Session)
import String
import UI.Elements as El
import Validate exposing (Valid, Validator)



-- MODEL


type alias Model =
    { session  : Session
    , title    : String
    , errors   : List Error
    , attemptedSubmission : Bool
    , username : String
    , password : String
    }


type alias Error =
    ( FormField, String )


type FormField
    = Username
    | Password


emptyModel : Session -> Model
emptyModel session =
    { session = session
    , title = "Login"
    , errors = []
    , attemptedSubmission = False
    , username = ""
    , password = ""
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( emptyModel session
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
            let
                newModel =
                    setField model field value
            in
            case Validate.validate modelValidator newModel of
                Ok validForm ->
                    ( { newModel | errors = [] }
                    , Cmd.none
                    )

                Err errors ->
                    ( { newModel | errors = errors }
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
                    ( model
                    , Session.login userInfo
                    )

                Err errResponse ->
                    ( handleErrors model errResponse
                    , Cmd.none
                    )


setField : Model -> FormField -> String -> Model
setField model field value =
    case field of
        Username ->
            { model | username = value }

        Password ->
            { model | password = value }


credsFromValidForm : Valid Model -> Credentials
credsFromValidForm validForm =
    let
        model =
            Validate.fromValid validForm
    in
    Credentials model.username model.password


sendLogin : (Result Http.Error UserInfo -> msg) -> Credentials -> Cmd msg
sendLogin responseMsg creds =
    Http.send responseMsg (Api.Users.postLogin creds)


handleErrors : Model -> Http.Error -> Model
handleErrors model error =
    case error of
        Http.BadUrl url ->
            { model | session = Session.addNotification model.session ("Bad url: " ++ url) }

        Http.BadPayload _ _ ->
            { model | session = Session.addNotification model.session "Invalid data sent to server" }

        Http.Timeout ->
            { model | session = Session.addNotification model.session "Couldn't reach server" }

        Http.NetworkError ->
            { model | session = Session.addNotification model.session "Couldn't reach server" }

        Http.BadStatus statusResponse ->
            { model | session = Session.addNotification model.session ("Error: " ++ .body statusResponse) }



-- VIEW


view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        Scrollable <|
            El.titledContent "Sign in"
                [ Html.form
                    [ classList
                        [ ( "grid", True )
                        , ( "l-12", True )
                        , ( "s-12", True )
                        ]
                    , Events.onSubmit Submitted
                    ]
                    [ El.validatedInput Username "text" "Username" model.username FormFieldChanged True model.errors model.attemptedSubmission
                    , El.validatedInput Password "password" "Password" model.password FormFieldChanged True model.errors model.attemptedSubmission
                    , El.submitButton
                        [ ( "l-12", True )
                        , ( "right", True )
                        ]
                        "Sign in"
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
