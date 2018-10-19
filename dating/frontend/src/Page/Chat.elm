module Page.Chat exposing (..)
import Html exposing (Html)
import Skeleton
import Session
import Routing exposing (Route(..))
import Generated.DatingApi exposing (..)
import GenHelpers exposing (..)
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
    , userYou : User
    , userFriend: User
    }


type Content
    = Content (List Message)


emptyUser : User
emptyUser =
    User "" "" "" Other "" "" 0 "" ""

init : Session.Data -> Int -> Int -> ( Model, Cmd Msg )
init session idFriend idYou =
  ( Model (Debug.log "messages session:" session) "Messages" (Content [(Message "User1" 5 "Hi"), (Message "User2" 6 "Hello"), (Message "User1" 5 "What's up?")])
        emptyUser emptyUser
  , (sendGetUser HandleGetUser idYou (authenticationToken session) ))


-- UPDATE

type Msg
    = NoOp
    | HandleGetUser (Result Http.Error (User))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        HandleGetUser result ->
            case result of
                Ok fetchedUser ->
                    Debug.log (Debug.toString fetchedUser) ( {model | userYou = fetchedUser }, Cmd.none)

                Err errResponse ->
                    Debug.log (Debug.toString errResponse) ( { model | userFriend = emptyUser }, Cmd.none )


-- VIEW


view : Model -> Skeleton.Details msg
view model =
    { title = model.title
    , kids = [
        Element.column [ width (px 600), height shrink, centerY, centerX, spacing 10, padding 10 ]
            (viewContent model.content model)
    ]}

viewContent : Content -> Model -> List (Element msg)
viewContent (Content messages) model =
    List.map (viewMessages model) messages


viewMessages : Model -> Message -> Element msg
viewMessages model message =
    el [ padding 10, width (fill |> maximum 255), Border.width 2, Border.rounded 20,
         (getPosition message.userId model.userYou.userId), Font.center
       ] (text message.message)


getPosition : Int -> Int -> Attribute msg
getPosition idMessage idYou =
    case ((Debug.log "idYou:" idYou) == idMessage) of
        True ->
            Element.alignRight
        False ->
            Element.alignLeft


authenticationToken : Session.Data -> String
authenticationToken data =
    case data of
        Session.LoggedIn navKey token ->
            token
        Session.Guest navKey ->
            ""

sendGetUser : (Result Http.Error User -> msg) -> Int -> String -> Cmd msg
sendGetUser responseMsg userId token =
    Http.send responseMsg (getUsersByUserid userId token)


blue =
    Element.rgb 0.4 0.4 0.8