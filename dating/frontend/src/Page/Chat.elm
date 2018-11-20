module Page.Chat exposing (Model, Msg(..), init, subscriptions, update, view)

import Api.Messages exposing (Conversation, Message)
import Api.Users
import Browser.Dom as Dom exposing (Viewport)
import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Http
import Routing exposing (Route(..))
import Session as Session exposing (Details, PageType(..), Session)
import String
import Task
import Time
import UI.Elements as El
import List.Extra



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
    , kids = Scrollable
        <| El.titledContentLoader model.loaded
            ("Chatting with " ++ model.usernameFriend)
            [ Keyed.ul
                [ classList
                    [ ( "messages", True )
                    , ( "l-12", True )
                    , ( "l-6", True )
                    ]
                , Attributes.id listId
                ]
                (List.concat (List.map (viewMessageGroup model True) (List.Extra.groupWhile (\a b -> a.authorName == b.authorName) model.messages)))
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


viewMessageKeyed : Model -> Message -> Bool -> Bool -> ( String, Html msg )
viewMessageKeyed model message isFirst isLast =
    ( message.timeStamp
    , Lazy.lazy4 viewMessage model message isFirst isLast
    )


viewMessage : Model -> Message -> Bool -> Bool -> Html msg
viewMessage model message isFirst isLast = 
    let
        myMessage = model.usernameSelf == message.authorName
    in
    Html.li
        [ classList
            [ ( "message", True )
            , ( "is-first-in-group", isFirst )
            , ( "is-last-in-group", isLast )
            , ( "author-me", myMessage )
            , ( "author-friend", not myMessage ) ]
        ]
        [ div
            [ ]
            [ Html.text message.body ]
        ]

viewMessageGroup : Model -> Bool -> (Message, List Message) -> List ( String, Html Msg )
viewMessageGroup model isFirstMessage (firstMessage, restOfMessages) =
    case (List.length restOfMessages) of
        0 -> [(viewMessageKeyed model firstMessage isFirstMessage True)]
        _ ->
            let
                firstRestOfMessages = Maybe.withDefault firstMessage (List.head restOfMessages)
                lastRestOfMessages = Maybe.withDefault restOfMessages (List.tail restOfMessages)
                firstMsgHtml = viewMessageKeyed model firstMessage isFirstMessage ((List.length restOfMessages) == 0)
                lastMsgsHtml = viewMessageGroup model False (firstRestOfMessages, lastRestOfMessages)
            in
                firstMsgHtml::lastMsgsHtml
    

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
