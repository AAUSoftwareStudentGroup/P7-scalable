module Page.CreateUser exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (class, classList, style, attribute)
import Html.Events as Events exposing (onClick)
import Validate exposing (Validator, Valid)
import String
import Http
import Date exposing (Unit(..))

import Session exposing (Session, Details)
import Routing exposing (Route(..))
import Session exposing (Session, PageType(..))
import Ports.FileUploadPort exposing (FilePortData, fileSelected, fileContentRead)
import UI.Elements as El

import Api.Types exposing (Gender(..), Image)
import Api.Authentication exposing (UserInfo)
import Api.Users exposing (NewUser)



-- MODEL


type alias Model =
    { session   : Session
    , title     : String
    , errors    : List (Error)
    , attemptedSubmission : Bool
    , checkingUsername : Bool
    , usernameOk : Bool
    , email     : String
    , username  : String
    , password1 : String
    , password2 : String
    , gender    : Gender
    , birthday  : String
    , city      : String
    , bio       : String
    , mImage    : Maybe Image
    }


type alias Error =
    ( FormField, String )


type FormField
    = Email
    | Username
    | Password1
    | Password2
    | Birthday
    | City
    | Bio


init : Session -> ( Model, Cmd Msg )
init session =
    ( updateErrors (Model session "New user" [] False False False "" "" "" "" Male "" "" "" Nothing)
    , Cmd.none
    )


-- UPDATE


type Msg
    = FormFieldChanged FormField String
    | GenderChanged Gender
    | UsernameChecked (Result Http.Error Bool)
    | FileSelected
    | FileRead FilePortData
    | Submitted
    | HandleUserCreated (Result Http.Error UserInfo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormFieldChanged field value ->
            case field of
                Username ->
                    if value == "" then
                        ( updateErrors <| setField model field value
                        , Cmd.none
                        )
                    else
                        ( updateErrors <| setField { model | checkingUsername = True } field value
                        , sendCheckUsername UsernameChecked value
                        )
                _ ->
                    ( updateErrors <| setField model field value
                    , Cmd.none
                    )

        GenderChanged newGender ->
            ( { model | gender = newGender }, Cmd.none )

        UsernameChecked result ->
            case result of
                Ok alreadyExists ->
                    ( updateErrors <| { model | checkingUsername = False, usernameOk = not alreadyExists }
                    , Cmd.none
                    )
                Err _ ->
                    ( model
                    , Cmd.none
                    )

        FileSelected ->
            ( model
            , fileSelected ()
            )

        FileRead portData ->
            let
                image = Just (Image portData.contents portData.fileName)
            in
                if portData.error == "" then
                    ( { model | mImage = image }
                    , Cmd.none
                    )
                else
                    ( { model | mImage = Nothing, session = Session.addNotification model.session portData.error }
                    , Cmd.none
                    )

        Submitted ->
            case Validate.validate modelValidator model of
                Ok validForm ->
                    ( { model | errors = [] }
                    , sendCreateUser HandleUserCreated (userFromValidForm validForm)
                    )
                Err errors ->
                    ( { model | errors = errors, attemptedSubmission = True }
                    , Cmd.none
                    )

        HandleUserCreated result ->
            case result of
                Ok userInfo ->
                    ( model, Session.login userInfo)

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
        Email ->
            { model | email = value }
        Username ->
            { model | username = value }
        Password1 ->
            { model | password1 = value }
        Password2 ->
            { model | password2 = value }
        Birthday ->
            { model | birthday = value }
        City ->
            { model | city = value }
        Bio ->
            { model | bio = value }


updateErrors : Model -> Model
updateErrors model =
    case Validate.validate modelValidator model of
        Ok validForm ->
            { model | errors = [] }
        Err errors ->
            { model | errors = errors }

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    fileContentRead FileRead

userFromValidForm : Valid Model -> NewUser
userFromValidForm validForm =
    userFromModel (Validate.fromValid validForm)


userFromModel : Model -> NewUser
userFromModel { email, password1, username, gender, birthday, city, bio, mImage } =
    NewUser email password1 username gender birthday city bio (encodeMaybeImage mImage)

encodeMaybeImage : Maybe Image -> String
encodeMaybeImage mImg =
    case mImg of
        Just img ->
         case List.head <| List.drop 1 (String.split "base64," img.contents) of
             Just a -> a
             Nothing -> ""
        Nothing -> ""


sendCheckUsername : (Result Http.Error (Bool) -> msg) -> String -> Cmd msg
sendCheckUsername responseMsg username =
    Http.send responseMsg (Api.Users.getUserAlreadyExists username)


sendCreateUser : (Result Http.Error (UserInfo) -> msg) -> NewUser -> Cmd msg
sendCreateUser responseMsg user =
    Http.send responseMsg (Api.Users.postUsers user)


responseToString : Maybe String -> String
responseToString r =
    case r of
        Just msg ->
            msg

        Nothing ->
            "No messages"


-- VIEW

view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids = Scrollable
        <| El.titledContent model.title
            [ Html.form [ classList
                            [ ( "grid", True )
                            , ( "l-12", True )
                            ]
                        , Events.onSubmit Submitted
                        ]
                [ El.validatedInput Email "email" "Email" model.email FormFieldChanged True model.errors model.attemptedSubmission
                , El.asyncValidatedInput Username "text" "Username"  model.username FormFieldChanged True model.errors model.attemptedSubmission model.checkingUsername
                , El.validatedInput Password1 "password" "Password" model.password1 FormFieldChanged True model.errors model.attemptedSubmission
                , El.validatedInput Password2 "password" "Repeat password"  model.password2 FormFieldChanged True model.errors model.attemptedSubmission
                , El.validatedInput City "text" "City" model.city FormFieldChanged True model.errors model.attemptedSubmission
                , El.validatedInput Bio "text" "Description" model.bio FormFieldChanged True model.errors model.attemptedSubmission
                , El.validatedInput Birthday "date" "Birthday" model.birthday FormFieldChanged True model.errors model.attemptedSubmission
                , El.labelledRadio "Gender" GenderChanged model.gender
                    [ ( "Male", Male )
                    , ( "Female", Female )
                    , ( "Other", Other )
                    ]
                , El.imageInput "Profile picture" FileSelected model.mImage
                , El.submitButton "Sign up"
                ]
            ]
    }

-- VALIDATION

modelValidator : Validator ( FormField, String ) Model
modelValidator =
    Validate.all
        [ Validate.ifBlank .email ( Email, "Please enter an email" )
        , Validate.ifInvalidEmail .email (\_ -> ( Email, "Please enter a valid email" ))

        , Validate.ifBlank .username ( Username, "Please enter a username" )
        , Validate.ifFalse (\model -> model.usernameOk) ( Username, "Username already in use" )

        , Validate.ifBlank .password1 ( Password1, "Please enter a password" )
        , Validate.ifBlank .password2 ( Password2, "Please repeat your password" )
        , Validate.ifFalse (\model -> doPasswordsMatch model) ( Password2, "Passwords don't match" )

        , Validate.ifBlank .birthday ( Birthday, "Please enter your birthday" )
        , Validate.ifFalse (\model -> isDateValidFormat model ) ( Birthday, "Please enter birthday in a valid format" )
        , Validate.ifFalse (\model -> isDateValid model ) ( Birthday, "You must be at least 18 years old" )

        , Validate.ifBlank .city ( City, "Please enter your city" )

        , Validate.ifBlank .bio ( Bio, "Please write a short description" )
        ]


doPasswordsMatch : Model -> Bool
doPasswordsMatch model =
     model.password1 == model.password2


isDateValidFormat : Model -> Bool
isDateValidFormat model =
    case Date.fromIsoString model.birthday of
        Ok _  -> True
        Err _ -> False
        
isDateValid : Model -> Bool
isDateValid model =
    case Date.fromIsoString model.birthday of
        Ok date  -> 
            (Date.diff Years date <| Session.getNow model.session) >= 18
        Err _ -> False
