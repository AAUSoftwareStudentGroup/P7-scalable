module Page.CreateUser exposing (Model, Msg(..), init, subscriptions, update, view)

import DatingApi as Api exposing (Gender(..), User)
import Html exposing (Html, div)
import Html.Attributes as Attributes
import Html.Events as Events exposing (onClick)
import Validate exposing (Validator, Valid)
import String
import Http

import Session exposing (Session, Details)
import Routing exposing (Route(..))
import Session exposing (Session)
import UI.Elements as El



-- MODEL


type alias Model =
    { session : Session
    , title : String
    , response : Maybe String
    , errors : List (Error)
    , email : String
    , username : String
    , password1 : String
    , password2 : String
    , gender : Gender
    , birthday : String
    , city : String
    , bio : String
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
    ( Model session "" Nothing [] "" "" "" "" Male "" "" ""
    , Cmd.none
    )


-- UPDATE


type Msg
    = FormFieldChanged FormField String
    | GenderChanged Gender
    | Submitted
    | HandleUserCreated (Result Http.Error Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormFieldChanged field value ->
            ( setField model field value, Cmd.none )

        GenderChanged newGender ->
            ( { model | gender = newGender }, Cmd.none )

        Submitted ->
            case Validate.validate modelValidator model of
                Ok validForm ->
                    ( { model | errors = [] }
                    , sendCreateUser HandleUserCreated (userFromValidForm validForm)
                    )
                Err errors ->
                    ( { model | errors = errors }
                    , Cmd.none
                    )

        HandleUserCreated result ->
            case result of
                Ok uid ->
                    ( model, Routing.replaceUrl (Session.getNavKey model.session) (Routing.routeToString Login) )

                Err errResponse ->
                    case errResponse of
                        Http.BadUrl url ->
                            ( { model | response = Just <| "Bad url: " ++ url }, Cmd.none )

                        Http.BadPayload _ _ ->
                            ( { model | response = Just "bad payload" }, Cmd.none )

                        Http.Timeout ->
                            ( { model | response = Just "timeout" }, Cmd.none )

                        Http.NetworkError ->
                            ( { model | response = Just "networkerror" }, Cmd.none )

                        Http.BadStatus statusResponse ->
                            ( { model | response = Just <| "badstatus" ++ .body statusResponse }, Cmd.none )

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


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


userFromValidForm : Valid Model -> User
userFromValidForm validForm =
    userFromModel (Validate.fromValid validForm)

userFromModel : Model -> User
userFromModel { email, password1, username, gender, birthday, city, bio } =
    User email password1 username gender birthday city 0 bio "token"


createUserCmd : User -> Cmd Msg
createUserCmd user =
    Http.send HandleUserCreated (Api.postUsers user)



-- VIEW

view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        El.contentWithHeader "New user"
            [ Html.form [ Events.onSubmit Submitted ]
                [ El.validatedInput Email "email" "Email" "Email" model.email FormFieldChanged model.errors
                , El.validatedInput Username "text" "Username" "Username" model.username FormFieldChanged model.errors
                , El.validatedInput Password1 "password" "Password" "Password" model.password1 FormFieldChanged model.errors
                , El.validatedInput Password2 "password" "Repeat password" "Password" model.password2 FormFieldChanged model.errors
                , El.labelledRadio "Gender" GenderChanged model.gender
                    [ ( "Male", Male )
                    , ( "Female", Female )
                    , ( "Other", Other )
                    ]
                , El.validatedInput Birthday "text" "Birthday" "YYYY-MM-DD" model.birthday FormFieldChanged model.errors
                , El.validatedInput City "text" "City" "City" model.city FormFieldChanged model.errors
                , El.validatedInput Bio "multiline" "Description" "Short description of yourself" model.bio FormFieldChanged model.errors
                , El.submitButton "Sign up"
                ]
            , Html.text (responseToString model.response)
            ]
    }

-- VALIDATION

modelValidator : Validator ( FormField, String ) Model
modelValidator =
    Validate.all
        [ Validate.ifBlank .email ( Email, "Email can't be blank." )
        , Validate.ifInvalidEmail .email (\_ -> ( Email, "Email is invalid." ))

        , Validate.ifBlank .username ( Username, "Username can't be blank." )
        , Validate.ifFalse (\model -> isUsernameValid model) ( Username, "Username already in use")

        , Validate.ifBlank .password1 ( Password1, "Password can't be blank." )
        , Validate.ifBlank .password2 ( Password2, "Repeated password can't be blank." )
        , Validate.ifFalse (\model -> doPasswordsMatch model) ( Password1, "Passwords don't match")
        , Validate.ifFalse (\model -> doPasswordsMatch model) ( Password2, "Passwords don't match")

        , Validate.ifBlank .birthday ( Birthday, "Birthday can't be blank." )
        , Validate.ifFalse (\model -> isDateValid model) ( Birthday, "Invalid format")

        , Validate.ifBlank .city ( City, "City can't be blank." )

        , Validate.ifBlank .bio ( Bio, "Description can't be blank." )
        ]




responseToString : Maybe String -> String
responseToString r =
    case r of
        Just msg ->
            msg

        Nothing ->
            "No messages"


isUsernameValid : Model -> Bool
isUsernameValid model =
    if model.username == "Bargsteen" then
        False
    else
        True


doPasswordsMatch : Model -> Bool
doPasswordsMatch model =
     model.password1 == model.password2


isDateValid : Model -> Bool
isDateValid model =
    let
        date = model.birthday
        dateLength = String.length date
        year = Maybe.withDefault -1 (String.toInt (String.slice 0 4 date))
        month = Maybe.withDefault -1 (String.toInt (String.slice 5 7 date))
        day = Maybe.withDefault -1 (String.toInt (String.slice 8 10 date))

        separators = String.slice 4 5 date ++ String.slice 7 8 date

        isYearNice = year >= 0 && year <= 2018
        isMonthNice = month >= 0 && month <= 12
        isDayNice = day >= 0 && day <= 31
        isSeparatorsNice = separators == "--"
    in
        dateLength == 10 && isYearNice && isSeparatorsNice && isMonthNice && isDayNice


sendCreateUser : (Result Http.Error Int -> msg) -> User -> Cmd msg
sendCreateUser responseMsg user =
    Http.send responseMsg (Api.postUsers user)
