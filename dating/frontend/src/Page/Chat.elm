module Page.Chat exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Html.Keyed exposing (ul)
import Http
import String as String
import Task as Task
import Time as Time

import Api.Messages exposing (Message, Conversation)
import Api.Users

import Routing exposing (Route(..))
import Session as Session exposing (Session, Details)
import UI.Elements as El

-- MODEL

startPage = 0
messagesPerPage = 100

type alias Model =
    { session           : Session
    , title             : String
    , loaded            : Bool
    , content           : List Message
    , usernameFriend    : String
    , usernameSelf      : String
    , numMsgs           : Int
    , unsentMessage     : String
    , zone              : Time.Zone
    , time              : Time.Posix
    }


emptyModel : Session -> Model
emptyModel session =
    Model session "Messages" False [] "" "" 0 "" Time.utc (Time.millisToPosix 0)


init : Session -> String -> ( Model, Cmd Msg )
init session usernameFriend =
    case session of
        Session.Guest _ _ ->
            ( emptyModel session
            , Routing.goHome (Session.getNavKey session)
            )
        Session.LoggedIn _ _ userInfo ->
            ( Model session "Messages" False [] usernameFriend userInfo.username 0 "" Time.utc (Time.millisToPosix 0)
            , getMessagesOrRedirect session userInfo.username usernameFriend
            )

getMessagesOrRedirect : Session -> String -> String -> Cmd Msg
getMessagesOrRedirect session usernameSelf usernameFriend =
    if usernameSelf == usernameFriend then
        Routing.replaceUrl (Session.getNavKey session) (Routing.routeToString Home)
    else
        Cmd.batch
            [ Task.perform AdjustTimeZone Time.here
            , Task.perform FetchMessages Time.now
            ]

-- UPDATE

type Msg
    = AdjustTimeZone Time.Zone
    | FetchMessages Time.Posix
    | HandleFetchedMessages (Result Http.Error Conversation)
    | UnsentMessageChanged String
    | SendMessage
    | HandleMessageSent (Result Http.Error (String.String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        FetchMessages newTime ->
            case (model.session) of
                Session.Guest _ _->
                    ( { model | time = newTime }, Cmd.none)
                Session.LoggedIn _ _ userInfo ->
                    ( { model | time = newTime }
                    , Http.send HandleFetchedMessages
                        (Api.Messages.getMessagesFromUsername userInfo model.usernameFriend model.numMsgs messagesPerPage)
                    )

        HandleFetchedMessages result ->
            case result of
                Ok conversation ->
                    ( { model | content = List.append model.content conversation.messages
                      , loaded = True
                      , numMsgs = model.numMsgs + List.length conversation.messages
                      }, Cmd.none)
                Err _ ->
                    ( model, Cmd.none )

        UnsentMessageChanged new ->
            ( { model | unsentMessage = new }, Cmd.none )

        SendMessage ->
            ( model, sendMessage model )

        HandleMessageSent result ->
            case result of
                Ok responseString ->
                    ( { model | unsentMessage = "" }, Task.perform FetchMessages Time.now )
                Err _ ->
                    ( model , Cmd.none  )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every 3000 FetchMessages ]

-- VIEW


view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        El.titledContentLoader model.loaded ("Chatting with " ++ model.usernameFriend)
            [ ul
                [ classList
                    [ ( "grid", True )
                    , ( "l-12", True )
                    , ( "l-6", True )
                    ]
                ]
                (List.map (viewMessage model) model.content)
            , Html.form
                [ Events.onSubmit SendMessage
                , classList
                      [ ( "l-12", True )
                      , ( "l-6", True )
                      ]
                ]
                [ El.simpleInput "text" "message" model.unsentMessage UnsentMessageChanged
                , El.submitButtonHtml
                    [ El.iconText "Send" "send" ]
                ]
            ]
    }

viewMessage : Model -> Message -> (String, Html Msg)
viewMessage model message =
    let
        myMessage = model.usernameSelf == message.authorName
    in
        ( message.timeStamp
        , Html.li [ classList
                    [ ( "message", True )
                    , ( "l-12", True )
                    , ( "author-me", myMessage)
                    , ( "author-friend", not myMessage)
                    ]
                  ]
            [Html.text message.body]
        )


sendMessage : Model -> Cmd Msg
sendMessage model =
    if (String.isEmpty model.unsentMessage) then
        Cmd.none
    else
        case model.session of
            Session.LoggedIn _ _ userInfo ->
                Http.send HandleMessageSent (Api.Messages.postMessage userInfo model.unsentMessage model.usernameFriend)
            Session.Guest _ _ ->
                Cmd.none


