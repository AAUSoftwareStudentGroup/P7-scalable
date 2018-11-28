module Page.Messages exposing (Model, Msg(..), init, subscriptions, update, view)

import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (class, classList)
import Html.Events as Events
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Browser.Dom as Dom
import Http
import Task as Task
import Time as Time
import List.Extra

import UI.Elements as El
import Session exposing (Session, PageType(..), Details)
import Routing exposing (Route(..))
import Api.Messages exposing (Message, emptyConvoPreview, emptyMessage, Conversation, ConversationPreview)
import Ports.LoadMorePort exposing (LoadMoreData, loadMore)


-- MODEL
type alias Model =
    { session       : Session
    , title         : String
    , loaded        : Bool
    , usernameSelf  : String
    , unsentMessage : String
    , attemptedSend : Bool
    , previews      : List ConversationPreview
    , chattingWith  : String
    , convos        : Dict String (Bool, List Message) -- (Done, List of messages)
    , loadingConvo  : Bool
    }

initModel : Session -> Model
initModel session =
    Model session "Messages" False "" "" False [] "" Dict.empty False


init : Session -> Maybe String -> ( Model, Cmd Msg )
init session initUsername =
    let
        model = initModel session
    in
        case session of
            Session.Guest _ _ _ ->
                ( model
                , Routing.goToLogin (Session.getNavKey session)
                )
            Session.LoggedIn _ _ _ userInfo ->
                ( { model | usernameSelf = userInfo.username, chattingWith = Maybe.withDefault "" initUsername }
                , sendGetConvos HandleInitConvos model
                )

-- UPDATE
type Msg
    = NoOp
    | GetConvos Time.Posix
    | GetNewMessages Time.Posix
    | ConvoSelected String
    | HandleInitConvos (Result Http.Error (List ConversationPreview))
    | HandleGetConvos (Result Http.Error (List ConversationPreview))
    | HandleGetInitMessages (Result Http.Error Conversation)
    | HandleGetOldMessages (Result Http.Error Conversation)
    | HandleGetNewMessages (Result Http.Error Conversation)
    | UnsentMessageChanged String
    | SendMessage
    | HandleMessageSent (Result Http.Error String.String)
    | LoadMore Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetConvos _ ->
            case (model.session) of
                Session.Guest _ _ _ ->
                    ( model, Cmd.none)
                Session.LoggedIn _ _ _ userInfo ->
                    ( model, sendGetConvos HandleGetConvos model)

        GetNewMessages _ ->
            case (model.session) of
                Session.Guest _ _ _ ->
                    ( model, Cmd.none)
                Session.LoggedIn _ _ _ userInfo ->
                    ( model, sendGetMessages HandleGetNewMessages model.chattingWith model 0 pageSize)

        HandleInitConvos result ->
            case result of
                Ok fetchedConvos ->
                    if List.length fetchedConvos == 0 && model.chattingWith == ""then
                        ( { model | previews = [], loaded = True }
                        , Cmd.none )
                    else
                        let
                            sortedConvos = List.sortWith sortConvos fetchedConvos
                            username =
                                if model.chattingWith == "" then
                                    (Maybe.withDefault emptyConvoPreview (List.head sortedConvos)).convoWithUsername
                                else
                                    model.chattingWith

                            finalConvos = prependPreviewIfNotExists sortedConvos username
                        in
                            ( { model | previews = finalConvos, chattingWith = username, loaded = True, loadingConvo = True }
                            , sendGetMessages HandleGetInitMessages username model 0 pageSize )

                Err errResponse ->
                    ( model, Cmd.none )

        HandleGetConvos result ->
            case result of
                Ok fetchedConvos ->
                    if List.length fetchedConvos == 0 && model.chattingWith == ""then
                        ( { model | previews = [], loaded = True }
                        , Cmd.none )
                    else
                        let
                            sortedConvos = List.sortWith sortConvos fetchedConvos
                            finalConvos = prependPreviewIfNotExists sortedConvos model.chattingWith
                        in
                            ( { model | previews = finalConvos }
                            , Cmd.none )

                Err errResponse ->
                    ( model, Cmd.none )

        ConvoSelected username ->
            let
                command =
                    case Dict.get username model.convos of
                        Nothing ->
                            sendGetMessages HandleGetInitMessages username model 0 pageSize
                        Just _ ->
                            jumpToBottom listId
            in
                ( { model | chattingWith = username }, command )


        HandleGetInitMessages result ->
            case result of
                Ok fetchedConvo ->
                    let
                        username = fetchedConvo.convoWithUsername
                        messages = fetchedConvo.messages
                        gottenAllMessages = (List.length messages < pageSize)

                        command =
                            if (List.length messages) == 0 then
                                Cmd.none
                            else
                                jumpToBottom listId
                    in
                        ( { model | convos = Dict.insert username (gottenAllMessages, messages) model.convos, loaded = True, loadingConvo = False }
                        , command)

                Err errResponse ->
                    ( model, Cmd.none )


        HandleGetNewMessages result ->
            case result of
                Ok fetchedConvo ->
                    let
                        username = fetchedConvo.convoWithUsername
                        oldest = Maybe.withDefault emptyMessage (List.head (List.reverse (listCurrentMessages model)))
                        newMessages = List.filter (\m -> compareMessage m oldest == LT) fetchedConvo.messages

                        newConvos =
                            case Dict.get username model.convos of
                                Nothing ->
                                    Dict.insert username (False, newMessages) model.convos
                                Just (done, oldMessageList) ->
                                    Dict.insert username (done, oldMessageList ++ newMessages) model.convos

                        command =
                            if (List.length newMessages) == 0 then
                                Cmd.none
                            else
                                jumpToBottom listId
                    in
                        ( { model | convos = newConvos, loaded = True }, command)

                Err errResponse ->
                    ( model, Cmd.none )

        HandleGetOldMessages result ->
            case result of
                Ok fetchedConvo ->
                    let
                        username = fetchedConvo.convoWithUsername
                        oldestMessage = Maybe.withDefault emptyMessage (List.head (listCurrentMessages model))
                        newMessages = List.filter (\m -> compareMessage m oldestMessage == GT) fetchedConvo.messages

                        gottenAllMessages = (List.length newMessages < pageSize)

                        newConvos =
                            case Dict.get username model.convos of
                                Nothing ->
                                    Dict.insert username (gottenAllMessages, newMessages) model.convos
                                Just (_, oldMessageList) ->
                                    Dict.insert username (gottenAllMessages, newMessages ++ oldMessageList) model.convos

                    in
                        ( { model | convos = newConvos, loaded = True, loadingConvo = False }
                        , Cmd.none)

                Err errResponse ->
                    ( model, Cmd.none )


        UnsentMessageChanged new ->
            ( { model | unsentMessage = new }, Cmd.none )

        SendMessage ->
            if model.attemptedSend || String.isEmpty model.unsentMessage then
                ( model, Cmd.none )
            else
                ( { model | attemptedSend = True }, sendMessage model )

        HandleMessageSent result ->
            case result of
                Ok responseString ->
                    ( { model | unsentMessage = "", attemptedSend = False }
                    , sendGetMessages HandleGetNewMessages model.chattingWith model 0 pageSize )

                Err _ ->
                    ( { model | attemptedSend = False }, Cmd.none )

        LoadMore _ ->
            ( { model | loadingConvo = True }
            , sendGetMessages HandleGetOldMessages model.chattingWith model (numberCurrentMessages model) pageSize
            )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 3000 GetConvos
        , Time.every 3000 GetNewMessages
        , loadMore LoadMore
        ]


-- VIEW
view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids = Fixed
        <| El.titledContentLoader model.loaded "Messages" <|
            if model.previews == [] then
                [ viewNoMessages ]
            else
                [ div
                    [ class "messaging-wrapper" ]
                    [ div
                        [ class "convos" ]
                        [ Keyed.ul
                            [ class "convo-list" ]
                            (List.map (viewConvoKeyed model) model.previews)
                        ]
                    , div
                        [ class "messages" ]
                        [ Keyed.ul
                            [ class "message-list"
                            , Attributes.id listId
                            ]
                            (viewTopElement model :: (List.concat (List.map (viewMessageGroup model True) (List.Extra.groupWhile (\a b -> a.authorName == b.authorName) (listCurrentMessages model)))))
                        , Html.form
                            [ Events.onSubmit SendMessage
                            , class "message-input"
                            ]
                            [ El.simpleInput "text" "Message" model.unsentMessage UnsentMessageChanged False
                            , El.submitButtonHtml
                                [ El.iconText "" "send" ]
                            ]
                        ]
                    ]
                ]
    }

viewNoMessages : Html Msg
viewNoMessages =
    div
    [ classList
        [ ( "no-conversations", True )
        , ( "l-12", True )
        , ( "s-12", True )
        ]
    ]
    [ Html.text "You don't have any conversations. Go check your matches and find someone to chat with." ]

viewTopElement : Model -> (String, Html Msg)
viewTopElement model =
    let
        element =
            if Tuple.first (Maybe.withDefault (False, []) (Dict.get model.chattingWith model.convos)) then
                div
                    [ classList
                        [ ( "conversation-start", True ) ]
                    ]
                    [ Html.text ("This is the beginning of your conversation with " ++ model.chattingWith) ]
            else
                let
                    icon =
                        if model.loadingConvo then
                            "more_horiz"
                        else
                            "keyboard_arrow_up"
                in
                    El.msgButtonFlat
                        [ classList
                            [ ( "load-more-button", True )
                            , ( "loading", model.loadingConvo )
                            ]
                        ]
                        (LoadMore True)
                        [ El.iconText "Load more" icon ]
    in
        ( "first-element"
        , element
        )


viewConvoKeyed : Model -> ConversationPreview -> (String, Html Msg)
viewConvoKeyed model message =
    ( message.convoWithUsername
    , Lazy.lazy2 viewConvo model message
    )


viewConvo : Model -> ConversationPreview -> Html Msg
viewConvo model message =
    let
        chattingWith = message.convoWithUsername == model.chattingWith
    in
        Html.li
            [ classList
                [ ( "conversation", True )
                , ( "active", chattingWith )
                ]
            , Attributes.attribute "attr-id" <| message.convoWithUsername
            , Events.onClick (ConvoSelected message.convoWithUsername)
            ]
            [ Html.span [ class "conversation-with" ]
                [ Html.text message.convoWithUsername ]
            , Html.span [ class "conversation-last-message" ]
                [ Html.text (lastMessage message) ]
            ]


viewMessageKeyed : Model -> Message -> Bool -> Bool -> ( String, Html msg )
viewMessageKeyed model message isFirst isLast =
    ( String.fromInt (Time.posixToMillis message.timeStamp)
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
    if List.length restOfMessages == 0 then
        [(viewMessageKeyed model firstMessage isFirstMessage True)]
    else
        let
            firstRestOfMessages = Maybe.withDefault firstMessage (List.head restOfMessages)
            lastRestOfMessages = Maybe.withDefault restOfMessages (List.tail restOfMessages)
            firstMsgHtml = viewMessageKeyed model firstMessage isFirstMessage ((List.length restOfMessages) == 0)
            lastMsgsHtml = viewMessageGroup model False (firstRestOfMessages, lastRestOfMessages)
        in
            firstMsgHtml::lastMsgsHtml



-- HELPERS
pageSize = 25

listId = "message-list"

numberCurrentMessages : Model -> Int
numberCurrentMessages model =
    List.length (listCurrentMessages model)


listCurrentMessages : Model -> List (Message)
listCurrentMessages model =
    Tuple.second (Maybe.withDefault (False, []) (Dict.get model.chattingWith model.convos))


jumpToBottom : String -> Cmd Msg
jumpToBottom id =
    Dom.getViewportOf id
        |> Task.andThen (\info -> Dom.setViewportOf id 0 info.scene.height)
        |> Task.attempt (\_ -> NoOp)


lastMessage : ConversationPreview -> String
lastMessage conversation =
    if conversation.isLastAuthor then
         "You: " ++ conversation.body
    else
        conversation.body


sortConvos : ConversationPreview -> ConversationPreview -> Order
sortConvos a b =
    compare (Time.posixToMillis b.timeStamp) (Time.posixToMillis a.timeStamp)


compareMessage : Message -> Message -> Order
compareMessage a b =
    compare (Time.posixToMillis b.timeStamp) (Time.posixToMillis a.timeStamp)

prependPreviewIfNotExists : List (ConversationPreview) -> String -> List (ConversationPreview)
prependPreviewIfNotExists convos username =
    if hasConvoWithUser convos username then
        convos
    else
        (newConvoPreview username) :: convos

hasConvoWithUser : List (ConversationPreview) -> String -> Bool
hasConvoWithUser convos username =
    (List.length <| List.filter (\convo -> convo.convoWithUsername == username) convos) == 1


newConvoPreview : String -> ConversationPreview
newConvoPreview username =
    ConversationPreview username "No messages" False <| Time.millisToPosix 0


sendGetConvos : (Result Http.Error (List ConversationPreview) -> msg) -> Model -> Cmd msg
sendGetConvos responseMsg model =
    case model.session of
        Session.LoggedIn _ _ _ userInfo ->
            Http.send responseMsg (Api.Messages.getConvoPreview userInfo)
        Session.Guest _ _ _ ->
            Cmd.none


sendGetMessages : (Result Http.Error Conversation -> msg) -> String -> Model -> Int -> Int -> Cmd msg
sendGetMessages responseMsg username model offset numMessages =
    case model.session of
        Session.LoggedIn _ _ _ userInfo ->
            Http.send responseMsg (Api.Messages.getMessagesFromUsername userInfo username (-1 * (offset + numMessages)) numMessages)

        Session.Guest _ _ _ ->
            Cmd.none


sendMessage : Model -> Cmd Msg
sendMessage model =
    case model.session of
        Session.LoggedIn _ _ _ userInfo ->
            Http.send HandleMessageSent (Api.Messages.postMessage userInfo model.unsentMessage model.chattingWith)

        Session.Guest _ _ _ ->
            Cmd.none
