module Page.CreateUser exposing (..)

import Html exposing (Html)
import Skeleton
import Session
import Generated.DatingApi exposing (..)
import GenHelpers exposing (Gender(..))
import Http
import Routing exposing (replaceUrl)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region


-- MODEL
type alias Model = 
    { session : Session.Data
    , title : String
    , response : Maybe String
    , email : String
    , username : String
    , password : String 
    , gender : Gender
    , birthday : String
    , town : String
    , profileText : String
    }

init : Session.Data -> ( Model, Cmd Msg )
init session = 
  ( Model session "" Nothing "" "" "" Other "" "" ""
  , Cmd.none
  )

-- UPDATE

type Msg
    = EntryChanged Model
    | CreateUserClicked
    | HandleUserCreated (Result Http.Error Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EntryChanged updatedModel ->
            (updatedModel, Cmd.none)

        CreateUserClicked ->
            ( model, createUserCmd <| mkUserFromEntries model )

        HandleUserCreated result ->
            case result of
                Ok uid ->
                    (model, Routing.replaceUrl (Session.navKey model.session) "login")

                Err errResponse ->
                    case errResponse of
                        Http.BadUrl url ->
                            ({model | response = Just <| "Bad url: " ++ url}, Cmd.none)

                        Http.BadPayload _ _ ->
                            ({model | response = Just "bad payload"}, Cmd.none)

                        Http.Timeout ->
                            ({model | response = Just "timeout"}, Cmd.none)

                        Http.NetworkError ->
                            ({model | response = Just "networkerror"}, Cmd.none)

                        Http.BadStatus statusResponse ->
                            ({model | response = Just <| "badstatus" ++ .body statusResponse}, Cmd.none)


mkUserFromEntries : Model -> User
mkUserFromEntries {email, password, username, gender, birthday, town, profileText}
    = User email password username gender birthday town profileText "token" 10

createUserCmd : User -> Cmd Msg
createUserCmd user =
    Http.send HandleUserCreated (postUsers user)


pure : Model -> ( Model, Cmd Msg )
pure model =
    ( model, Cmd.none )


-- VIEW

view : Model -> Skeleton.Details Msg
view model =
    { title = model.title
    , kids = [ viewContent model.title model ]
    }

viewContent : String -> Model -> Element Msg
viewContent title model =
            Element.column [ width (px 800), height shrink, centerY, centerX, spacing 36, padding 10 ]
                -- , explain Debug.todo ]
                [ el
                    [ Region.heading 1
                    , centerX
                    , Font.size 36
                    ]
                    (text "User creation")
                , Input.email
                    [ spacing 12 ]
                    { text = model.email
                    , placeholder = Nothing
                    , onChange = \new -> EntryChanged { model | email = new }
                    , label = Input.labelAbove [ Font.size 14 ] (text "Email")
                    }
                , Input.username
                    [ spacing 12
                    , below (showWarningIfUsernameIsTaken model.username)
                    ]
                    { text = model.username
                    , placeholder = Just (Input.placeholder [] (text "username"))
                    , onChange = \new -> EntryChanged { model | username = new }
                    , label = Input.labelAbove [ Font.size 14 ] (text "Username")
                    }
                , Input.newPassword [ spacing 12, width shrink ]
                    { text = model.password
                    , placeholder = Nothing
                    , onChange = \new -> EntryChanged { model | password = new }
                    , label = Input.labelAbove [ Font.size 14 ] (text "Password")
                    , show = False
                    }
                -- , Input.newPassword [ spacing 12, width shrink, below (maybeShowPasswordsNotEqualWarning model) ]
                --     { text = model.userPasswordAgain
                --     , placeholder = Nothing
                --     , onChange = \new -> EntryChanged { model | passwordAgain = new }
                --     , label = Input.labelAbove [ Font.size 14 ] (text "Repeat password")
                --     , show = False
                --     }
                , Input.radio
                    [ spacing 12
                    ]
                    { selected = Just model.gender
                    , onChange = \new -> EntryChanged { model | gender = new }
                    , label = Input.labelAbove [ Font.size 14, paddingXY 0 12 ] (text "Gender")
                    , options =
                        [ Input.option Male (text "Man")
                        , Input.option Female (text "Woman")
                        , Input.option Other (text "Other")
                        ]
                    }
                , Input.text [ spacing 12 ]
                    { text = model.birthday
                    , onChange = \new -> EntryChanged { model | birthday = new }
                    , placeholder = Nothing
                    , label = Input.labelAbove [ Font.size 14 ] (text "Birthday")
                    }
                , Input.text [ spacing 12 ]
                    { text = model.town
                    , onChange = \new -> EntryChanged { model | town = new }
                    , placeholder = Nothing
                    , label = Input.labelAbove [ Font.size 14 ] (text "Town")
                    }
                , Input.multiline
                    [ height shrink
                    , spacing 12

                    -- , padding 6
                    ]
                    { text = model.profileText
                    , placeholder = Just (Input.placeholder [] (text "I like big butts and I cannot lie."))
                    , onChange = \new -> EntryChanged { model | profileText = new }
                    , label = Input.labelAbove [ Font.size 14 ] (text "Describe yourself")
                    , spellcheck = False
                    }
                , Input.button
                    [ Background.color red
                    , Font.color white
                    , Border.color darkBlue
                    , paddingXY 32 16
                    , Border.rounded 3
                    , width fill
                    ]
                    { onPress = Just CreateUserClicked
                    , label = Element.text "Create!"
                    }
                , Element.text <| responseToString model.response
                ]


responseToString : Maybe String -> String
responseToString r =
    case r of
        Just msg ->
            msg

        Nothing ->
            "No messages"



showWarningIfUsernameIsTaken username =
    if username == "Bargsteen" then
        mkWarning "Username is taken"

    else
        none


mkWarning warning =
    el
        [ Font.color red
        , Font.size 14
        , alignRight
        , moveDown 6
        ]
        (text warning)


maybeShowPasswordsNotEqualWarning model =
    if model.userPasswordAgain /= "" && model.userPassword /= model.userPasswordAgain then
        mkWarning "Passwords do not match"

    else
        none


noLabel =
    Input.labelAbove [] none


white =
    Element.rgb 1 1 1


grey =
    Element.rgb 0.9 0.9 0.9


blue =
    Element.rgb 0 0 0.8


red =
    Element.rgb 0.8 0 0


darkBlue =
    Element.rgb 0 0 0.9
