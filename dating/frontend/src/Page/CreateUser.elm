module Page.CreateUser exposing (..)

import Html exposing (Html)
import Skeleton
import Session
import Generated.DatingApi exposing (..)
import GenHelpers exposing (Gender(..))
import Http

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input

-- MODEL
type alias Model = 
    { session : Session.Data
    , title : String
    , content : Content
    }

init : Session.Data -> ( Model, Cmd Msg )
init session = 
  ( Model session "Create User Title" (Content emptyUser Nothing)
  , Cmd.none
  )

emptyUser : User
emptyUser =
  User "kasper@bargsteen.com" "bargsteen" "repsak" Male "1994-05-06" "Aalborg" "Wuhu" "mySecretToken"

type Content
    = Content User ResponseString

type alias ResponseString =
    Maybe String

-- UPDATE

type Msg
    = Update User
    | CreateUserClicked
    | HandleUserCreated (Result Http.Error Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ -> pure model
        -- Update newFormEntries ->
        --     pure (Model newFormEntries response)

        -- CreateUserClicked ->
        --     ( Model user response, postUser user )

        -- HandleUserCreated result ->
        --     case result of
        --         Ok uid ->
        --             pure (Model user (Just <| String.fromInt uid))

        --         Err errResponse ->
        --             case errResponse of
        --                 Http.BadUrl url ->
        --                     pure (Model user (Just <| "Bad url: " ++ url))

        --                 Http.BadPayload _ _ ->
        --                     pure (Model user (Just "bad payload"))

        --                 Http.Timeout ->
        --                     pure (Model user (Just "timeout"))

        --                 Http.NetworkError ->
        --                     pure (Model user (Just "networkerror"))

        --                 Http.BadStatus statusResponse ->
        --                     pure (Model user (Just <| "badstatus" ++ .body statusResponse))


postUser : User -> Cmd Msg
postUser user =
    Http.send HandleUserCreated (postUsers user)


pure : Model -> ( Model, Cmd Msg )
pure userEntries =
    ( userEntries, Cmd.none )


-- VIEW

view : Model -> Skeleton.Details msg
view model =
    { title = model.title
    , kids = [ viewContent model.title model.content ]
    }

viewContent : String -> Content -> Html msg
viewContent title content =
    Element.layout [Font.size 20] <|
        column [] [ text "CreateUser CONTENT"
               , link [] {url = "messages", label = toText "To messages"}
        ]

toText : String -> Element msg
toText str = el [Font.size 20] <| text str
    -- { title = "Create User"
    -- , body =
    --     [ Element.layout
    --         [ Font.size 20
    --         ]
    --       <|
    --         Element.column [ width (px 800), height shrink, centerY, centerX, spacing 36, padding 10 ]
    --             -- , explain Debug.todo ]
    --             [ el
    --                 [ Region.heading 1
    --                 , centerX
    --                 , Font.size 36
    --                 ]
    --                 (text "User creation")
    --             , Input.email
    --                 [ spacing 12 ]
    --                 { text = userEntries.userEmail
    --                 , placeholder = Nothing
    --                 , onChange = \new -> Update { userEntries | userEmail = new }
    --                 , label = Input.labelAbove [ Font.size 14 ] (text "Email")
    --                 }
    --             , Input.username
    --                 [ spacing 12
    --                 , below (showWarningIfUsernameIsTaken userEntries)
    --                 ]
    --                 { text = userEntries.userUsername
    --                 , placeholder = Just (Input.placeholder [] (text "username"))
    --                 , onChange = \new -> Update { userEntries | userUsername = new }
    --                 , label = Input.labelAbove [ Font.size 14 ] (text "Username")
    --                 }
    --             , Input.newPassword [ spacing 12, width shrink ]
    --                 { text = userEntries.userPassword
    --                 , placeholder = Nothing
    --                 , onChange = \new -> Update { userEntries | userPassword = new }
    --                 , label = Input.labelAbove [ Font.size 14 ] (text "Password")
    --                 , show = False
    --                 }

    --             -- , Input.newPassword [ spacing 12, width shrink, below (maybeShowPasswordsNotEqualWarning userEntries) ]
    --             --     { text = userEntries.userPasswordAgain
    --             --     , placeholder = Nothing
    --             --     , onChange = \new -> Update { userEntries | userPasswordAgain = new }
    --             --     , label = Input.labelAbove [ Font.size 14 ] (text "Repeat password")
    --             --     , show = False
    --             --     }
    --             , Input.radio
    --                 [ spacing 12
    --                 ]
    --                 { selected = Just userEntries.userGender
    --                 , onChange = \new -> Update { userEntries | userGender = new }
    --                 , label = Input.labelAbove [ Font.size 14, paddingXY 0 12 ] (text "Gender")
    --                 , options =
    --                     [ Input.option Male (text "Man")
    --                     , Input.option Female (text "Woman")
    --                     , Input.option Other (text "Other")
    --                     ]
    --                 }
    --             , Input.text [ spacing 12 ]
    --                 { text = userEntries.userBirthday
    --                 , onChange = \new -> Update { userEntries | userBirthday = new }
    --                 , placeholder = Nothing
    --                 , label = Input.labelAbove [ Font.size 14 ] (text "Birthday")
    --                 }
    --             , Input.text [ spacing 12 ]
    --                 { text = userEntries.userTown
    --                 , onChange = \new -> Update { userEntries | userTown = new }
    --                 , placeholder = Nothing
    --                 , label = Input.labelAbove [ Font.size 14 ] (text "Town")
    --                 }
    --             , Input.multiline
    --                 [ height shrink
    --                 , spacing 12

    --                 -- , padding 6
    --                 ]
    --                 { text = userEntries.userProfileText
    --                 , placeholder = Just (Input.placeholder [] (text "I like big butts and I cannot lie."))
    --                 , onChange = \new -> Update { userEntries | userProfileText = new }
    --                 , label = Input.labelAbove [ Font.size 14 ] (text "Describe yourself")
    --                 , spellcheck = False
    --                 }
    --             , Input.button
    --                 [ Background.color red
    --                 , Font.color white
    --                 , Border.color darkBlue
    --                 , paddingXY 32 16
    --                 , Border.rounded 3
    --                 , width fill
    --                 ]
    --                 { onPress = Just CreateUserClicked
    --                 , label = Element.text "Create!"
    --                 }
    --             , Element.text <| responseToString response
    --             ]
    --     ]
    -- }


responseToString : ResponseString -> String
responseToString r =
    case r of
        Just msg ->
            msg

        Nothing ->
            "No messages"


showWarningIfUsernameIsTaken userEntries =
    if userEntries.userUsername == "Bargsteen" then
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


maybeShowPasswordsNotEqualWarning userEntries =
    if userEntries.userPasswordAgain /= "" && userEntries.userPassword /= userEntries.userPasswordAgain then
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

sendCreateUser : (Result Http.Error Int -> msg) -> User -> Cmd msg
sendCreateUser responseMsg user =
    Http.send responseMsg (postUsers user)
