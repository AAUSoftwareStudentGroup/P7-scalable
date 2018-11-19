module Page.Messages exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (class, classList)
import Html.Keyed as Keyed
import Html.Lazy as Lazy
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
    , loaded    : Bool
    , content   : List ConversationPreviewDTO
    , zone      : Time.Zone
    , time      : Time.Posix
    }

initModel : Session -> Model
initModel session =
    Model session "Messages" False [] Time.utc (Time.millisToPosix 0)

init : Session -> ( Model, Cmd Msg )
init session =
        case session of
            Session.Guest _ _ ->
                ( initModel session,
                Routing.goToLogin (Session.getNavKey session)
                )
            Session.LoggedIn _ _ _ ->
                ( initModel session
                , Cmd.batch
                    [ Task.perform AdjustTimeZone Time.here
                    , (sendGetMessages HandleGetMessages session)
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
                    ( { model | content = fetchedMessages, loaded = True }, Cmd.none)

                Err errResponse ->
                    ( model, Cmd.none )

        FetchMessages newTime ->
            case (model.session) of
                Session.Guest _ _ ->
                    ( { model | time = newTime }, Cmd.none)
                Session.LoggedIn _ _ userInfo ->
                    ( { model | time = newTime }
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
        El.titledContentLoader model.loaded "Messages"
            [ Keyed.ul
                [ classList
                    [ ( "messages", True )
                    , ( "l-12", True )
                    , ( "l-6", True )
                    ]
                ]
                (List.map viewMessageKeyed model.content)
            ]
    }


viewMessageKeyed : ConversationPreviewDTO -> (String, Html msg)
viewMessageKeyed message =
    ( message.convoWithUsername
    , Lazy.lazy viewMessage message
    )

viewMessage : ConversationPreviewDTO -> Html msg
viewMessage message =
    Html.li [ class "conversation", Attributes.attribute "attr-id" <| message.convoWithUsername ]
        [ Html.a [ Attributes.href (Routing.routeToString (Chat message.convoWithUsername)) ]
            [ div [ class "conversation-with" ]
                [ Html.text message.convoWithUsername ]
            , div [ class "conversation-last-message" ]
            [ Html.text (lastMessage message) ]
            ]
        ]

lastMessage : ConversationPreviewDTO -> String
lastMessage conversation =
    if conversation.isLastAuthor then
         "You: " ++ conversation.body
    else
        conversation.body

sendGetMessages : (Result Http.Error (List ConversationPreviewDTO) -> msg) -> Session -> Cmd msg
sendGetMessages responseMsg session =
    case session of
        Session.LoggedIn _ _ userInfo ->
            Http.send responseMsg (Api.Messages.getConvoPreview userInfo)
        Session.Guest _ _ ->
            Cmd.none
