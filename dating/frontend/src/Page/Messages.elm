module Page.Messages exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html)
import Session exposing (Session, Details)
import Routing exposing (Route(..))
import DatingApi exposing (getRecentMessages, Message)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Task as Task
import Time as Time


-- MODEL
type alias Model = 
    { session : Session
    , title : String
    , content : List Message
    , zone : Time.Zone
    , time : Time.Posix
    }


init : Session -> ( Model, Cmd Msg )
init session =
  ( Model (Debug.log "messages session:" session) "Messages" []
    Time.utc
    (Time.millisToPosix 0)
  , Cmd.batch [ (sendGetMessages HandleGetMessages session)
              , Task.perform AdjustTimeZone Time.here
              ]
  )


-- UPDATE
type Msg
    = NoOp
    | HandleGetMessages (Result Http.Error (List Message))
    | FetchMessages Time.Posix
    | AdjustTimeZone Time.Zone


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

        FetchMessages newTime ->
            case (model.session) of
                Session.Guest _ ->
                    ( { model | time = newTime }, Cmd.none)
                Session.LoggedIn _ userInfo ->
                    ( { model | time = (Debug.log "current time: " newTime) }
                    , sendGetMessages HandleGetMessages model.session
                    )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 3000 FetchMessages



-- VIEW
view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids = [ column [width (px 600), padding 20, spacing 20, Border.width 2, centerX, alignTop]
        <| (List.map viewMessage model.content)
    ]
    }


viewMessage : Message -> Element msg
viewMessage message =
    case (Debug.log "authorIsMe:" message.imLastAuthor) of
        False ->
            link [width fill]
            { url = Routing.routeToString <| (Chat message.convoWithId)
            , label = row [padding 20, spacing 20, Border.width 2, Background.color blue, width fill ]
                        [ el [ Font.size 20, width fill ] <| text message.convoWithUsername
                        , el [ Font.size 20, width fill, Background.color yellow ] <| text message.body
                        ]
            }
        True ->
            Element.none


sendGetMessages : (Result Http.Error (List Message) -> msg) -> Session -> Cmd msg
sendGetMessages responseMsg session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (getRecentMessages userInfo)
        Session.Guest _ ->
            Cmd.none


blue =
    Element.rgb 0.4 0.4 0.8

yellow =
    Element.rgb 0.8 0.8 0.2
