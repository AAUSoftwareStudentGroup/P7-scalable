module Page.Chat exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Html.Keyed exposing (ul)
import Http
import String as String
import Task as Task
import Time as Time

import DatingApi as Api exposing (User, Gender(..), ChatMessage, PostMessage)
import Routing exposing (Route(..))
import Session as Session exposing (Session, Details)
import UI.Elements as El

-- MODEL
type alias Model =
    { session : Session
    , title : String
    , content : List ChatMessage
    , idYou : Int
    , idFriend : Int
    , username : String
    , unsentMessage: String
    , zone : Time.Zone
    , time : Time.Posix
    }

init : Session -> Int -> ( Model, Cmd Msg )
init session idFriend =
    let
        idYou = Maybe.withDefault -1 (Session.getUserId session)
        username = Maybe.withDefault "" (Session.getUsername session)
    in
        ( Model session "Messages" [] idYou idFriend username "" Time.utc (Time.millisToPosix 0)
        , getMessagesOrRedirect session idYou idFriend
        )

getMessagesOrRedirect : Session -> Int -> Int -> Cmd Msg
getMessagesOrRedirect session idYou idFriend =
    if idYou == idFriend then
        Routing.replaceUrl (Session.getNavKey session) (Routing.routeToString Home)
    else
        Cmd.batch
            [ Task.perform AdjustTimeZone Time.here
            , Task.perform FetchMessages Time.now
            ]

-- UPDATE
type Msg
    = NoOp
    | AdjustTimeZone Time.Zone
    | FetchMessages Time.Posix
    | HandleFetchedMessages (Result Http.Error (List ChatMessage))
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
                Session.Guest _ ->
                    ( { model | time = newTime }, Cmd.none)
                Session.LoggedIn _ userInfo ->
                    ( { model | time = newTime }
                    , Http.send HandleFetchedMessages (Api.getMessagesFromId userInfo model.idFriend)
                    )

        HandleMessageSent result ->
            case result of
                Ok responseString ->
                    ( { model | content = addMessageToList model, unsentMessage = "" }, Cmd.none )
                Err _ ->
                    ( model , Cmd.none  )

        UnsentMessageChanged new ->
            ( { model | unsentMessage = new }, Cmd.none )

        SendMessage ->
            ( model, sendMessage model )

        HandleFetchedMessages result ->
            case result of
                Ok messages ->
                    ( { model | content = messages }, Cmd.none)
                Err _ ->
                    ( model, Cmd.none )


addMessageToList : Model -> List (ChatMessage)
addMessageToList model =
    (ChatMessage model.unsentMessage model.idYou 0 model.username (toUtcString model.time model.zone)) :: model.content


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
        El.contentWithHeader ("Chatting with " ++ model.username)
            [ ul
                [ classList
                    [ ( "grid", True )
                    , ( "l-12", True )
                    , ( "l-6", True )
                    ]
                ]
                (List.reverse (List.map (viewMessage model) model.content))
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


viewMessage : Model -> ChatMessage -> (String, Html Msg)
viewMessage model message =
    let
        myMessage = model.idYou == message.authorId
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
            Session.LoggedIn _ userInfo ->
                Http.send HandleMessageSent (Api.postMessage userInfo (createMessage model) model.idFriend)
            Session.Guest _ ->
                Cmd.none


createMessage : Model -> PostMessage
createMessage { idYou, username, time, zone, unsentMessage} =
    PostMessage 0 idYou username (toUtcString time zone) unsentMessage


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
