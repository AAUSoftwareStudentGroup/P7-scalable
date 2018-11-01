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
import Api.Messages exposing (Message)
import Api.Users

import Routing exposing (Route(..))
import Session as Session exposing (Session, Details)
import UI.Elements as El

-- MODEL
type alias Model =
    { session           : Session
    , title             : String
    , content           : List Message
    , idYou             : Int
    , idFriend          : Int
    , username          : String
    , unsentMessage     : String
    , zone              : Time.Zone
    , time              : Time.Posix
    }

init : Session -> Int -> ( Model, Cmd Msg )
init session idFriend =
  ( Model (Debug.log "messages session:" session)
    "Messages"
    []
    (Maybe.withDefault -1 (Session.getUserId session))
    idFriend
    (Maybe.withDefault "" (Session.getUsername session))
    ""
    Time.utc
    (Time.millisToPosix 0)
  , Cmd.batch [ Task.perform AdjustTimeZone Time.here
              , case (idFriend == (Maybe.withDefault -1 <| Session.getUserId session)) of
                    False ->
                        Task.perform FetchMessages Time.now
                    True ->
                        case session of
                            Session.LoggedIn key _ ->
                                Routing.replaceUrl key (Routing.routeToString Home)
                            Session.Guest key ->
                                Routing.replaceUrl key (Routing.routeToString Home)
              ]
  )



-- UPDATE
type Msg
    = NoOp
    | UnsentMessageChanged String
    | SubmitMessage
    | HandleMessageSent (Result Http.Error (String.String))
    | FetchMessages Time.Posix
    | HandleFetchedMessages (Result Http.Error (List Message))
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UnsentMessageChanged new ->
            ( { model | unsentMessage = new }, Cmd.none )

        SubmitMessage ->
            ( model, sendMessage model )

        HandleMessageSent result ->
            case (Debug.log "response: "result) of
                Ok responseString ->
                    ( { model | unsentMessage = "" }
                    , Cmd.none)
                Err _ ->
                    ( model , Cmd.none  )

        FetchMessages newTime ->
            case (model.session) of
                Session.Guest _ ->
                    ( { model | time = newTime }, Cmd.none)
                Session.LoggedIn _ userInfo ->
                    ( { model | time = (Debug.log "current time: " newTime) }
                    , Http.send HandleFetchedMessages (Api.Messages.getMessagesFromId userInfo model.idFriend)
                    )

        HandleFetchedMessages result ->
            case result of
                Ok messages ->
                    ( { model | content = messages }, Cmd.none)
                Err _ ->
                    (model, Cmd.none)

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
                [ Events.onSubmit SubmitMessage
                , classList
                      [ ( "l-12", True )
                      , ( "l-6", True )
                      ]
                ]
                [ El.simpleInput "text" "message" model.unsentMessage UnsentMessageChanged
                , El.submitButton "Send"
                ]
            ]
    }

viewMessage : Model -> Message -> (String, Html Msg)
viewMessage model message =
    let
        myMessage = model.username == message.authorName
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
                Http.send HandleMessageSent (Api.Messages.postMessage userInfo model.unsentMessage model.idFriend )
            Session.Guest _ ->
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
