module Page.CreateUser exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (class, classList)
import Html.Events as Events exposing (onClick)
import Validate exposing (Validator, Valid)
import String
import Http

import Session exposing (Session, Details)
import Routing exposing (Route(..))
import Session exposing (Session)
import UI.Elements as El
import Api.Types exposing (Gender(..), UserInfo)
import Api.Users exposing (NewUser)



-- MODEL


type alias Model =
    { session : Session
    , title : String
    , errors : List (Error)
    , attemptedSubmission: Bool
    , email : String
    , username : String
    , password1 : String
    , password2 : String
    , gender    : Gender
    , birthday  : String
    , city      : String
    , bio       : String
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
    ( updateErrors (Model session "New user" [] False "" "" "" "" Male "" "" "")
    , Cmd.none
    )


-- UPDATE


type Msg
    = FormFieldChanged FormField String
    | GenderChanged Gender
    | Submitted
    | HandleUserCreated (Result Http.Error UserInfo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormFieldChanged field value ->
            ( updateErrors <| setField model field value
            , Cmd.none
            )

        GenderChanged newGender ->
            ( { model | gender = newGender }, Cmd.none )

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
    Sub.none

userFromValidForm : Valid Model -> NewUser
userFromValidForm validForm =
    userFromModel (Validate.fromValid validForm)


userFromModel : Model -> NewUser
userFromModel { email, password1, username, gender, birthday, city, bio } =
    NewUser email password1 username gender birthday city bio


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
    , kids =
        El.titledContent model.title
            [ Html.form [ classList
                            [ ( "grid", True )
                            , ( "l-12", True )
                            ]
                        , Events.onSubmit Submitted
                        ]
                [ El.validatedInput Email "email" "Email" model.email FormFieldChanged model.errors model.attemptedSubmission
                , El.validatedInput Username "text" "Username"  model.username FormFieldChanged model.errors model.attemptedSubmission
                , El.validatedInput Password1 "password" "Password" model.password1 FormFieldChanged model.errors model.attemptedSubmission
                , El.validatedInput Password2 "password" "Repeat password"  model.password2 FormFieldChanged model.errors model.attemptedSubmission
                , El.validatedInput City "text" "City" model.city FormFieldChanged model.errors model.attemptedSubmission
                , El.validatedInput Bio "text" "Description" model.bio FormFieldChanged model.errors model.attemptedSubmission
                , El.validatedInput Birthday "date" "Birthday" model.birthday FormFieldChanged model.errors model.attemptedSubmission
                , El.labelledRadio "Gender" GenderChanged model.gender
                    [ ( "Male", Male )
                    , ( "Female", Female )
                    , ( "Other", Other )
                    ]
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
        , Validate.ifFalse (\model -> isUsernameValid model) ( Username, "Username already in use" )

        , Validate.ifBlank .password1 ( Password1, "Please enter a password" )
        , Validate.ifBlank .password2 ( Password2, "Please repeat your password" )
        , Validate.ifFalse (\model -> doPasswordsMatch model) ( Password2, "Passwords don't match" )

        , Validate.ifBlank .birthday ( Birthday, "Please enter your birthday" )
        , Validate.ifFalse (\model -> isDateValidFormat model ) ( Birthday, "Please enter birthday in a valid format" )
        , Validate.ifFalse (\model -> isDateValid model ) ( Birthday, "You must be at least 18 years old" )

        , Validate.ifBlank .city ( City, "Please enter your city" )

        , Validate.ifBlank .bio ( Bio, "Please write a short description" )
        ]


isUsernameValid : Model -> Bool
isUsernameValid model =
    if model.username == "Bargsteen2" then
        False
    else
        True


doPasswordsMatch : Model -> Bool
doPasswordsMatch model =
     model.password1 == model.password2


isDateValidFormat : Model -> Bool
isDateValidFormat model =
    let
        date = model.birthday
        dateLength = String.length date
        year = Maybe.withDefault -1 (String.toInt (String.slice 0 4 date))
        month = Maybe.withDefault -1 (String.toInt (String.slice 5 7 date))
        day = Maybe.withDefault -1 (String.toInt (String.slice 8 10 date))

        separators = String.slice 4 5 date ++ String.slice 7 8 date

        isYearNice = year >= 0 && year <= 9999
        isMonthNice = month >= 0 && month <= 12
        isDayNice = day >= 0 && day <= 31
        isSeparatorsNice = separators == "--"
    in
        dateLength == 10 && isYearNice && isSeparatorsNice && isMonthNice && isDayNice


isDateValid : Model -> Bool
isDateValid model =
    let
        date = model.birthday
        year = Maybe.withDefault -1 (String.toInt (String.slice 0 4 date))
        month = Maybe.withDefault -1 (String.toInt (String.slice 5 7 date))
        day = Maybe.withDefault -1 (String.toInt (String.slice 8 10 date))
    in
        year <= 2000
