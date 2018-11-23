module Page.Messages exposing (Model, Msg(..), init, subscriptions, update, view)

import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (class, classList)
import Html.Events as Events
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Http
import Task as Task
import Time as Time
import List.Extra

import UI.Elements as El
import Session exposing (Session, PageType(..), Details)
import Routing exposing (Route(..))
import Api.Messages exposing (Message, Conversation, ConversationPreviewDTO)


-- MODEL
type alias Model =
    { session       : Session
    , title         : String
    , loaded        : Bool
    , usernameSelf  : String
    , unsentMessage : String
    , previews      : List ConversationPreviewDTO
    , convoShown    : String
    , convos        : Dict String (List Message)
    }

initModel : Session -> Model
initModel session =
    Model session "Messages" False "" "" [] "" Dict.empty


init : Session -> ( Model, Cmd Msg )
init session =
    let
        model = initModel session
    in
        case session of
            Session.Guest _ _ ->
                ( model
                , Routing.goToLogin (Session.getNavKey session)
                )
            Session.LoggedIn _ _ userInfo ->
                ( { model | usernameSelf = userInfo.username }
                , sendGetConvos HandleGetConvos model
                )

-- UPDATE
type Msg
    = NoOp
    | GetConvos Time.Posix
    | HandleGetConvos (Result Http.Error (List ConversationPreviewDTO))
    | ConvoSelected String
    | GetMessages String Time.Posix
    | HandleGetMessages (Result Http.Error Conversation)
    | UnsentMessageChanged String
    | SendMessage
    | HandleMessageSent (Result Http.Error String.String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GetConvos _ ->
            case (model.session) of
                Session.Guest _ _ ->
                    ( model, Cmd.none)
                Session.LoggedIn _ _ userInfo ->
                    ( model, sendGetConvos HandleGetConvos model)

        HandleGetConvos result ->
            case result of
                Ok fetchedConvos ->
                    let
                        sortedConvos = List.sortWith sortConvos fetchedConvos
                        username =
                            if model.convoShown == "" then
                                case List.head sortedConvos of
                                    Nothing ->
                                        ""
                                    Just convo ->
                                        convo.convoWithUsername
                            else
                                model.convoShown
                    in
                        ( { model | previews = sortedConvos, convoShown = username, loaded = True }
                        , sendGetMessages HandleGetMessages username model )

                Err errResponse ->
                    ( model, Cmd.none )

        ConvoSelected username ->
            ( { model | convoShown = username }, Cmd.none )

        GetMessages username _ ->
            ( model, sendGetMessages HandleGetMessages username model)

        HandleGetMessages result ->
            case result of
                Ok fetchedConvo ->
                    let
                        username = fetchedConvo.convoWithUsername
                        messages = fetchedConvo.messages
                    in
                        ( { model | convos = Dict.insert username messages model.convos, loaded = True }, Cmd.none)

                Err errResponse ->
                    ( model, Cmd.none )

        UnsentMessageChanged new ->
            ( { model | unsentMessage = new }, Cmd.none )

        SendMessage ->
            ( model, sendMessage model )

        HandleMessageSent result ->
            case result of
                Ok responseString ->
                    ( { model | unsentMessage = "" }
                    , sendGetMessages HandleGetMessages model.convoShown model )

                Err _ ->
                    ( model, Cmd.none )



sortConvos : ConversationPreviewDTO -> ConversationPreviewDTO -> Order
sortConvos a b =
    compare (Time.posixToMillis b.timeStamp) (Time.posixToMillis a.timeStamp)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 3000 GetConvos


-- VIEW
view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids = Fixed
        <| El.titledContentLoader model.loaded "Messages"
            [ div
                [ classList
                    [ ( "grid", True )
                    ]
                ]
                [ Keyed.ul
                        [ classList
                            [ ( "convos", True )
                            , ( "l-12", True )
                            , ( "l-6", True )
                            ]
                        ]
                        (List.map viewConvoKeyed model.previews)
                , div
                    [ classList
                        [ ( "chat", True )
                        ]
                    ]
                    [ Keyed.ul
                        [ classList
                            [ ( "messages", True )
                            , ( "l-12", True )
                            , ( "l-6", True )
                            ]
                        ]
                        (List.concat (List.map (viewMessageGroup model True) (List.Extra.groupWhile (\a b -> a.authorName == b.authorName) (listCurrentMessages model))))
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
                ]
            ]
    }

listCurrentMessages : Model -> List (Message)
listCurrentMessages model =
    Maybe.withDefault [] (Dict.get model.convoShown model.convos)

viewConvoKeyed : ConversationPreviewDTO -> (String, Html Msg)
viewConvoKeyed message =
    ( message.convoWithUsername
    , Lazy.lazy viewConvo message
    )

viewConvo : ConversationPreviewDTO -> Html Msg
viewConvo message =
    Html.li [ class "conversation", Attributes.attribute "attr-id" <| message.convoWithUsername ]
        [ Html.div [ Events.onClick (ConvoSelected message.convoWithUsername) ]
            [ div [ class "conversation-with" ]
                [ Html.text message.convoWithUsername ]
            , div [ class "conversation-last-message" ]
            [ Html.text (lastMessage message) ]
            ]
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



lastMessage : ConversationPreviewDTO -> String
lastMessage conversation =
    if conversation.isLastAuthor then
         "You: " ++ conversation.body
    else
        conversation.body

sendGetConvos : (Result Http.Error (List ConversationPreviewDTO) -> msg) -> Model -> Cmd msg
sendGetConvos responseMsg model =
    case model.session of
        Session.LoggedIn _ _ userInfo ->
            Http.send responseMsg (Api.Messages.getConvoPreview userInfo)
        Session.Guest _ _ ->
            Cmd.none

sendGetMessages : (Result Http.Error Conversation -> msg) -> String -> Model -> Cmd msg
sendGetMessages responseMsg username model =
    case model.session of
        Session.LoggedIn _ _ userInfo ->
            Http.send responseMsg (Api.Messages.getMessagesFromUsername userInfo username 0 1000)

        Session.Guest _ _ ->
            Cmd.none

sendMessage : Model -> Cmd Msg
sendMessage model =
    if String.isEmpty model.unsentMessage then
        Cmd.none
    else
        case model.session of
            Session.LoggedIn _ _ userInfo ->
                Http.send HandleMessageSent (Api.Messages.postMessage userInfo model.unsentMessage model.convoShown)

            Session.Guest _ _ ->
                Cmd.none
