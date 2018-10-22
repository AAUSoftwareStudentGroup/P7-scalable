module Page.Messages exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html)
import Skeleton
import Session exposing (Session)
import Routing exposing (Route(..))
import DatingApi exposing (getMessages, Message)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http


-- MODEL
type alias Model = 
    { session : Session
    , title : String
    , content : List Message
    }


init : Session -> ( Model, Cmd Msg )
init session =
  ( Model (Debug.log "messages session:" session) "Messages" [(Message "User1" 5 "Hi"), (Message "User2" 6 "Hello"), (Message "User1" 5 "What's up?")]
  , (sendGetMessages HandleGetMessages session)
  )


-- UPDATE
type Msg
    = NoOp
    | HandleGetMessages (Result Http.Error (List Message))
    | SessionChanged Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        HandleGetMessages result ->
            case result of
                Ok fetchedMessages ->
                    Debug.log (Debug.toString fetchedMessages) ( {model | content = fetchedMessages }, Cmd.none)

                Err errResponse ->
                    Debug.log (Debug.toString errResponse) ( model, Cmd.none )
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
    , kids = [ column [width (px 600), padding 20, spacing 20, Border.width 2, centerX]
        <| (List.map viewMessage model.content)
    ]
    }


viewMessage : Message -> Element msg
viewMessage message =
    row [padding 20, spacing 20, Border.width 2, Background.color blue, width fill ]
    [ el [ Font.size 20, width fill ] <| text message.username
    , el [ Font.size 20, width fill, Background.color yellow ] <| text message.message
    ]



sendGetMessages : (Result Http.Error (List Message) -> msg) -> Session -> Cmd msg
sendGetMessages responseMsg session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (getMessages userInfo)
        Session.Guest _ ->
            Cmd.none


blue =
    Element.rgb 0.4 0.4 0.8

yellow =
    Element.rgb 0.8 0.8 0.2
