module Page.Chat exposing (Model, Msg(..), init, subscriptions, update, view)

import Html exposing (Html, div)
import Html.Attributes as Attributes
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
    | SessionChanged Session
    | FetchMessages Time.Posix
    | HandleFetchedMessages (Result Http.Error (List ChatMessage))
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
                    ( { model | content = (
                        [(ChatMessage model.unsentMessage model.idYou 0 (toUtcString model.time model.zone))] ++
                        model.content
                        )
                        , unsentMessage = ""
                      }
                    , Cmd.none)
                Err _ ->
                    ( model , Cmd.none  )

        SessionChanged session ->
            case session of
                Session.Guest key ->
                    ( { model | session = session }
                    , Routing.replaceUrl key (Routing.routeToString Home)
                    )
                Session.LoggedIn key _ ->
                    ( { model | session = session }
                    , Routing.replaceUrl key (Routing.routeToString ListUsers)
                    )

        FetchMessages newTime ->
            case (model.session) of
                Session.Guest _ ->
                    ( { model | time = newTime }, Cmd.none)
                Session.LoggedIn _ userInfo ->
                    ( { model | time = (Debug.log "current time: " newTime) }
                    , Http.send HandleFetchedMessages (Api.getMessagesFromId userInfo model.idFriend)
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
    Sub.batch
        [ Session.onChange SessionChanged (Session.getNavKey model.session)
        , Time.every 3000 FetchMessages
        ]




-- VIEW
view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        [ div []
            [ ul []
                (List.reverse (List.map (viewMessage model) model.content))
            , Html.form [ Events.onSubmit SubmitMessage ]
                [ El.simpleInput "text" "message" model.unsentMessage UnsentMessageChanged
                , El.submitButton "Send"
                ]
            ]
        ]
    }



viewMessage : Model -> ChatMessage -> (String, Html Msg)
viewMessage model message =
    (message.timeStamp, Html.li [] [Html.text message.body])


sendMessage : Model -> Cmd Msg
sendMessage model =
    if (String.isEmpty model.unsentMessage) then
        Cmd.none
    else
        case model.session of
            Session.LoggedIn _ userInfo ->
                Http.send HandleMessageSent (Api.postMessage userInfo (Debug.log "message " (createMessage model)) model.idFriend )
            Session.Guest _ ->
                Cmd.none

createMessage : Model -> PostMessage
createMessage { idYou, time, zone, unsentMessage} =
    PostMessage 0 idYou (toUtcString time zone) unsentMessage

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
