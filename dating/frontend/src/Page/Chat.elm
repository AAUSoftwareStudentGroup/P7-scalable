module Page.Chat exposing (Model, Msg(..), init, subscriptions, update, view)

import Api.Messages exposing (Conversation, Message)
import Api.Users
import Browser.Dom as Dom exposing (Viewport)
import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Html.Keyed exposing (ul)
import Http
import Routing exposing (Route(..))
import Session as Session exposing (Details, Session)
import String
import Task
import Time
import UI.Elements as El



-- MODEL


startPage =
    0


messagesPerPage =
    100


listId =
    "message-list"


type alias Model =
    { session : Session
    , title : String
    , loaded : Bool
    , messages : List Message
    , numMessages : Int
    , usernameSelf : String
    , usernameFriend : String
    , unsentMessage : String
    }


emptyModel : Session -> Model
emptyModel session =
    Model session "Messages" False [] 0 "" "" ""


init : Session -> String -> ( Model, Cmd Msg )
init session usernameFriend =
    let
        redirect =
            ( emptyModel session
            , Routing.goToLogin (Session.getNavKey session)
            )
    in
    case session of
        Session.Guest _ _ ->
            redirect

        Session.LoggedIn _ _ userInfo ->
            if userInfo.username == usernameFriend then
                redirect

            else
                ( Model session "Messages" False [] 0 userInfo.username usernameFriend ""
                , Task.perform FetchMessages Time.now
                )



-- UPDATE


type Msg
    = NoOp
    | FetchMessages Time.Posix
    | HandleFetchedMessages (Result Http.Error Conversation)
    | UnsentMessageChanged String
    | SendMessage
    | HandleMessageSent (Result Http.Error String.String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FetchMessages newTime ->
            case model.session of
                Session.Guest _ _ ->
                    ( model, Cmd.none )

                Session.LoggedIn _ _ userInfo ->
                    ( model
                    , Http.send HandleFetchedMessages
                        (Api.Messages.getMessagesFromUsername userInfo model.usernameFriend model.numMessages messagesPerPage)
                    )

        HandleFetchedMessages result ->
            case result of
                Ok conversation ->
                    let
                        numNewMessages =
                            List.length conversation.messages

                        command =
                            if numNewMessages == 0 then
                                Cmd.none

                            else
                                jumpToBottom listId
                    in
                    ( { model
                        | messages = List.append model.messages conversation.messages
                        , numMessages = model.numMessages + numNewMessages
                        , loaded = True
                      }
                    , command
                    )

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
                    ( model, Cmd.none )



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
        El.titledContentLoader model.loaded
            ("Chatting with " ++ model.usernameFriend)
            [ ul
                [ classList
                    [ ( "messages", True )
                    , ( "l-12", True )
                    , ( "l-6", True )
                    ]
                , Attributes.id listId
                ]
                (List.map (viewMessage model) model.messages)
            , Html.form
                [ Events.onSubmit SendMessage
                , classList
                    [ ( "l-12", True )
                    , ( "l-6", True )
                    ]
                ]
                [ El.simpleInput "text" "message" model.unsentMessage UnsentMessageChanged False
                , El.submitButtonHtml
                    [ El.iconText "Send" "send" ]
                ]
            ]
    }


viewMessage : Model -> Message -> ( String, Html Msg )
viewMessage model message =
    let
        myMessage =
            model.usernameSelf == message.authorName
    in
    ( message.timeStamp
    , Html.li
        [ classList
            [ ( "message", True ) ]
        ]
        [ div
            [ classList
                [ ( "author-me", myMessage )
                , ( "author-friend", not myMessage )
                ]
            ]
            [ Html.text message.body ]
        ]
    )


sendMessage : Model -> Cmd Msg
sendMessage model =
    if String.isEmpty model.unsentMessage then
        Cmd.none

    else
        case model.session of
            Session.LoggedIn _ _ userInfo ->
                Http.send HandleMessageSent (Api.Messages.postMessage userInfo model.unsentMessage model.usernameFriend)

            Session.Guest _ _ ->
                Cmd.none


jumpToBottom : String -> Cmd Msg
jumpToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)
