module Page.Chat exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Html.Keyed exposing (ul)
import Http
import String as String
import Task as Task
import Time as Time

--import DatingApi as Api exposing (User, Gender(..), ChatMessage, PostMessage)
import Api.Messages exposing (Message, Conversation)
import Api.Users

import Routing exposing (Route(..))
import Session as Session exposing (Session, Details)
import UI.Elements as El

-- MODEL
type alias Model =
    { session           : Session
    , title             : String
    , loaded            : Bool
    , content           : List Message
    , localContent      : List Message
    , usernameFriend    : String
    , usernameSelf      : String
    , numMsgs           : Int
    , unsentMessage     : String
    , zone              : Time.Zone
    , time              : Time.Posix
    }


init : Session -> String -> ( Model, Cmd Msg )
init session usernameFriend =
    let
        username = Maybe.withDefault "" (Session.getUsername session)
    in
        ( Model session "Messages" False [] [] usernameFriend username 0 "" Time.utc (Time.millisToPosix 0)
        , getMessagesOrRedirect session username usernameFriend
        )

getMessagesOrRedirect : Session -> String -> String -> Cmd Msg
getMessagesOrRedirect session usernameSelf usernameFriend =
    if usernameSelf == usernameFriend then
        Routing.replaceUrl (Session.getNavKey session) (Routing.routeToString Home)
    else
        Cmd.batch
            [ Task.perform AdjustTimeZone Time.here
            , Task.perform FetchLocalMessages Time.now
            ]

-- UPDATE
type Msg
    = NoOp
    | AdjustTimeZone Time.Zone
    | FetchMessages Time.Posix
    | FetchLocalMessages Time.Posix
    | HandleFetchedMessages (Result Http.Error Conversation)
    | UnsentMessageChanged String
    | SendMessage
    | HandleMessageSent (Result Http.Error (String.String))

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        FetchMessages newTime ->
            case (model.session) of
                Session.Guest _ _->
                    ( { model | time = newTime }, Cmd.none)
                Session.LoggedIn _ _ userInfo ->
                    ( { model | time = newTime }
                    , Http.send HandleFetchedMessages
                        (Api.Messages.getMessagesFromUsername userInfo model.usernameFriend model.numMsgs)
                    )

        FetchLocalMessages newTime ->
            case (model.session) of
                Session.Guest _ _ ->
                    ( { model | time = newTime }, Cmd.none )
                Session.LoggedIn _ _ userInfo ->
                    ( { model | time = newTime }
                    , Http.send HandleFetchedMessages (Api.Messages.getMessagesFromUsername
                        userInfo model.usernameFriend model.numMsgs)
                    )


        HandleMessageSent result ->
            case result of
                Ok responseString ->
                    ( { model | localContent = addMessageToList model, unsentMessage = "" }, Cmd.none )
                Err _ ->
                    ( model , Cmd.none  )

        UnsentMessageChanged new ->
            ( { model | unsentMessage = new }, Cmd.none )

        SendMessage ->
            ( model, sendMessage model )

        HandleFetchedMessages result ->
            case result of
                Ok conversation ->
                    ( { model | content = List.append model.content conversation.messages
                      , loaded = True
                      , numMsgs = model.numMsgs + List.length conversation.messages
                      , localContent = []
                      }, Cmd.none)
                Err _ ->
                    ( model, Cmd.none )


addMessageToList : Model -> List Message
addMessageToList model =
    model.localContent ++ List.singleton (Message model.unsentMessage (toUtcString model.time model.zone) model.usernameSelf)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [ Time.every 3000 FetchMessages
    ]

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
                (List.map (viewMessage model) (List.append model.content model.localContent))
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













toUtcString : Time.Posix -> Time.Zone -> String
toUtcString time zone =
    String.fromInt (Time.toYear zone time)
    ++ "-" ++
    (case (Time.toMonth zone time) of
        Time.Jan -> "01"
        Time.Feb -> "02"
        Time.Mar -> "03"
        Time.Apr -> "04"
        Time.May -> "05"
        Time.Jun -> "06"
        Time.Jul -> "07"
        Time.Aug -> "08"
        Time.Sep -> "09"
        Time.Oct -> "10"
        Time.Nov -> "11"
        Time.Dec -> "12"
    )
    ++ "-" ++
    (case (Time.toDay zone time) < 10 of
        True ->
            "0" ++ String.fromInt (Time.toDay zone time)
        False ->
            String.fromInt (Time.toDay zone time)
    )
    ++ "T" ++

    (case (Time.toHour zone time) < 10 of
        True ->
            "0" ++ String.fromInt (Time.toHour zone time)
        False ->
            String.fromInt (Time.toHour zone time)
    )
    ++ ":" ++

    (case (Time.toMinute zone time) < 10 of
        True ->
            "0" ++ String.fromInt (Time.toMinute zone time)
        False ->
            String.fromInt (Time.toMinute zone time)
    )
    ++ ":" ++

    (case (Time.toSecond zone time) < 10 of
        True ->
            "0" ++ String.fromInt (Time.toSecond zone time)
        False ->
            String.fromInt (Time.toSecond zone time)
    )
    ++ "Z"
