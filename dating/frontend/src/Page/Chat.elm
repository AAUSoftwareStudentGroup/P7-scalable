module Page.Chat exposing (..)
import Html exposing (Html)
import Skeleton
import Session exposing (Session)
import Routing exposing (Route(..))
import DatingApi as Api exposing (User, Gender(..), Message)
import Http
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


-- MODEL
type alias Model =
    { session : Session
    , title : String
    , content : Content
    , idYou : Int
    , userFriend: User
    }


type Content
    = Content (List Message)


emptyUser : User
emptyUser =
    User "" "" "" Other "" "" 0 "" ""

init : Session -> Int -> ( Model, Cmd Msg )
init session idFriend =
  ( Model (Debug.log "messages session:" session)
    "Messages"
    (Content [(Message "User1" 5 "Hi"), (Message "User2" 6 "Hello"), (Message "User1" 5 "What's up?")])
    (Maybe.withDefault -1 (Session.getUserId session))
    emptyUser
  , (sendGetUser HandleGetUser idFriend session ))


-- UPDATE

type Msg
    = NoOp
    | HandleGetUser (Result Http.Error (User))
    | SessionChanged Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        HandleGetUser result ->
            case result of
                Ok fetchedUser ->
                    Debug.log (Debug.toString fetchedUser) ( {model | userFriend = fetchedUser }, Cmd.none)

                Err errResponse ->
                    Debug.log (Debug.toString errResponse) ( { model | userFriend = emptyUser }, Cmd.none )
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




-- VIEW


view : Model -> Skeleton.Details msg
view model =
    { title = model.title
    , session = model.session
    , kids = [
        Element.column [ width (px 600), height shrink, centerY, centerX, spacing 10, padding 10 ]
            <| (viewContent model.content model)
    ]}

viewContent : Content -> Model -> List (Element msg)
viewContent (Content messages) model =
    List.map (viewMessages model) messages


viewMessages : Model -> Message -> Element msg
viewMessages model message =
    el [ padding 10, width (fill |> maximum 255), Border.width 2, Border.rounded 20,
         (getPosition message.userId model.idYou), Font.center
       ] (text message.message)


getPosition : Int -> Int -> Attribute msg
getPosition idMessage idYou =
    case ((Debug.log "idYou:" idYou) == idMessage) of
        True ->
            Element.alignRight
        False ->
            Element.alignLeft


sendGetUser : (Result Http.Error User -> msg) -> Int -> Session -> Cmd msg
sendGetUser responseMsg userId session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (Api.getUserById userId userInfo)
        Session.Guest _ ->
            Cmd.none


blue =
    Element.rgb 0.4 0.4 0.8