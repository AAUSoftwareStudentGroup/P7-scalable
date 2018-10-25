module Page.Messages exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html, div)
import Session exposing (Session, Details)
import Routing exposing (Route(..))
import DatingApi exposing (getRecentMessages, Message)

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

  {-  Time.utc
    (Time.millisToPosix 0)
  , Cmd.batch [ Task.perform AdjustTimeZone Time.here
              , Task.perform FetchMessages Time.now
              ]
-}
-- UPDATE
type Msg
    = NoOp
    | HandleGetMessages (Result Http.Error (List Message))
    | SessionChanged Session
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
    Sub.batch [ Session.onChange SessionChanged (Session.getNavKey model.session)
              , Time.every 3000 FetchMessages
              ]



-- VIEW
view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        [ div []
            (List.map viewMessage model.content)
        ]
    }


viewMessage : Message -> Html msg
viewMessage message =
    div []
        [ Html.text message.convoWith
        , Html.text message.body
        ]


sendGetMessages : (Result Http.Error (List Message) -> msg) -> Session -> Cmd msg
sendGetMessages responseMsg session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (getRecentMessages userInfo)
        Session.Guest _ ->
            Cmd.none
