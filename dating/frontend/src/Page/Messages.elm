module Page.Messages exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (class, classList)
import Html.Keyed as Keyed
import Http
import Task as Task
import Time as Time
import UI.Elements as El


import Session exposing (Session, Details)
import Routing exposing (Route(..))
import Api.Messages exposing (ConversationPreviewDTO)

-- MODEL
type alias Model = 
    { session   : Session
    , title     : String
    , content   : List ConversationPreviewDTO
    , zone      : Time.Zone
    , time      : Time.Posix
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
    | HandleGetMessages (Result Http.Error (List ConversationPreviewDTO))
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
    , kids =
        El.contentWithHeader "Messages"
            [ Keyed.ul
                [ classList
                    [ ( "messages", True )
                    , ( "l-12", True )
                    , ( "l-6", True )
                    ]
                ]
                (List.map viewMessage model.content)
            ]
    }



viewMessage : ConversationPreviewDTO -> (String, Html msg)
viewMessage message =
    ( message.convoWithUsername
    , Html.li [ class "conversation", Attributes.attribute "attr-id" <| message.convoWithUsername ]
        [ div [ class "conversation-with" ]
            [ Html.text message.convoWithUsername ]
        , div [ class "conversation-last-message" ]
            [ Html.text message.body ]
        ]
    )


sendGetMessages : (Result Http.Error (List ConversationPreviewDTO) -> msg) -> Session -> Cmd msg
sendGetMessages responseMsg session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (Api.Messages.getConvoPreview userInfo)
        Session.Guest _ ->
            Cmd.none
