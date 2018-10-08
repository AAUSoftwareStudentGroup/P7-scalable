module Page.CreateUser exposing (Content(..), Model, Msg(..), ResponseString, blue, darkBlue, emptyUser, grey, init, initialModel, maybeShowPasswordsNotEqualWarning, mkWarning, noLabel, postUser, pure, red, responseToString, sendCreateUser, showWarningIfUsernameIsTaken, toText, update, view, viewContent, white)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import GenHelpers exposing (Gender(..))
import Generated.DatingApi exposing (..)
import Html exposing (Html)
import Http
import Routing exposing (Route(..), routeToString)
import Skeleton
import String



-- MODEL


type alias Model =
    { title : String
    , content : Content
    }


init : String -> String -> ( Model, Cmd Msg )
init title url =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    Model "Create User" (Content emptyUser Nothing)


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
        _ ->
            pure model


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
    Element.layout [ Font.size 20 ] <|
        column []
            [ text "CreateUser CONTENT"
            , link [] { url = Routing.routeToString Routing.Messages, label = toText "To messages" }
            , link [] { url = Routing.routeToString Routing.ListUsers, label = toText "All users" }
            ]


toText : String -> Element msg
toText str =
    el [ Font.size 20 ] <| text str


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
