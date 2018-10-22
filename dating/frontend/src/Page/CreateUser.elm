module Page.CreateUser exposing (Model, Msg(..), init, subscriptions, update, view)

import DatingApi as Api exposing (Gender(..), User)
import Element as Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import String
import Http

import DatingApi as Api exposing (User, Gender(..))
import Session exposing (Session, Details)
import Routing exposing (Route(..))
import Session exposing (Session)
import UI.Elements as El
import UI.Styles exposing (formInputStyle, formLabelStyle, acceptButtonStyle, centeredFillStyle)



-- MODEL


type alias Model =
    { session : Session
    , title : String
    , response : Maybe String
    , email : String
    , username : String
    , password : String
    , passwordAgain : String
    , gender : Gender
    , birthday : String
    , town : String
    , profileText : String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session "" Nothing "" "" "" "" Other "" "" ""
    , Cmd.none
    )



-- UPDATE


type Msg
    = EntryChanged Model
    | CreateUserClicked
    | HandleUserCreated (Result Http.Error Int)
    | SessionChanged Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EntryChanged updatedModel ->
            ( updatedModel, Cmd.none )

        CreateUserClicked ->
            ( model, createUserCmd <| mkUserFromEntries model )

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


mkUserFromEntries : Model -> User
mkUserFromEntries { email, password, username, gender, birthday, town, profileText } =
    User email password username gender birthday town 0 profileText "token"


createUserCmd : User -> Cmd Msg
createUserCmd user =
    Http.send HandleUserCreated (Api.postUsers user)



-- VIEW

view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        El.pageContent "New user" <|
            [ El.formColumn <|
                [ Input.email
                    (formInputStyle <| El.conditional (El.warning "Email invalid") (showEmailWarning model))
                    { text = model.email
                    , placeholder = El.placeholder "Email"
                    , onChange = \new -> EntryChanged { model | email = new }
                    , label = El.formLabel "Email"
                    }
                , Input.username
                    (formInputStyle <| El.conditional (El.warning "Username taken") (showUsernameWarning model))
                    { text = model.username
                    , placeholder = El.placeholder "Username"
                    , onChange = \new -> EntryChanged { model | username = new }
                    , label = El.formLabel "Username"
                    }
                , Input.newPassword
                    (formInputStyle Element.none)
                    { text = model.password
                    , placeholder = El.placeholder "Password"
                    , onChange = \new -> EntryChanged { model | password = new }
                    , label = El.formLabel "Password"
                    , show = False
                    }
                , Input.newPassword
                    (formInputStyle <| El.conditional (El.warning "Passwords do not match") (showPasswordWarning model))
                    { text = model.passwordAgain
                    , placeholder = El.placeholder "Repeat password"
                    , onChange = \new -> EntryChanged { model | passwordAgain = new }
                    , label = El.formLabel "Repeat password"
                    , show = False
                    }
                , Input.radio
                    (formInputStyle Element.none)
                    { selected = Just model.gender
                    , onChange = \new -> EntryChanged { model | gender = new }
                    , label = El.formLabel "Gender"
                    , options =
                        [ Input.option Male (text "Man")
                        , Input.option Female (text "Woman")
                        , Input.option Other (text "Other")
                        ]
                    }
                , Input.text
                    (formInputStyle <| El.conditional (El.warning "Invalid format. Should be YYYY-MM-DD") (showDateWarning model))
                    { text = model.birthday
                    , onChange = \new -> EntryChanged { model | birthday = new }
                    , placeholder = El.placeholder "YYYY-MM-DD"
                    , label = El.formLabel "Birthday"
                    }
                , Input.text
                    (formInputStyle Element.none)
                    { text = model.town
                    , onChange = \new -> EntryChanged { model | town = new }
                    , placeholder = El.placeholder "City"
                    , label = El.formLabel "City"
                    }
                , Input.multiline
                    (formInputStyle Element.none)
                    { text = model.profileText
                    , placeholder = El.placeholder "A short description of yourself"
                    , onChange = \new -> EntryChanged { model | profileText = new }
                    , label = El.formLabel "Description"
                    , spellcheck = True
                    }
                , El.messageButton (centeredFillStyle ++ acceptButtonStyle) CreateUserClicked "Sign up"
                , Element.text (responseToString model.response)
                ]
            ]
    }


responseToString : Maybe String -> String
responseToString r =
    case r of
        Just msg ->
            msg

        Nothing ->
            "No messages"

showEmailWarning : Model -> Bool
showEmailWarning model =
    model.email /= "" && (not (isEmailValid model))

isEmailValid : Model -> Bool
isEmailValid model =
    String.contains "@" model.email

showUsernameWarning : Model -> Bool
showUsernameWarning model =
    model.username /= "" && (not (isUsernameValid model))

isUsernameValid : Model -> Bool
isUsernameValid model =
    if model.username == "Bargsteen" then
        False
    else
        True

showPasswordWarning : Model -> Bool
showPasswordWarning model =
    not (doPasswordsMatch model)


doPasswordsMatch : Model -> Bool
doPasswordsMatch model =
     model.password == model.passwordAgain

showDateWarning : Model -> Bool
showDateWarning model =
    model.birthday /= "" && (not (isDateValid model))


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
