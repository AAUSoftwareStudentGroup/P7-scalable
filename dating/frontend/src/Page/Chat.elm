module Page.Chat exposing (Model, Msg(..), init, subscriptions, update, view)
import Html exposing (Html)
import Session as Session exposing (Session, Details)
import Routing exposing (Route(..))
import DatingApi as Api exposing (User, Gender(..), Message, PostMessage)
import Http
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Events as Events
import String as String
import Task as Task
import Time as Time


-- MODEL
type alias Model =
    { session : Session
    , title : String
    , content : List Message
    , idYou : Int
    , idFriend : Int
    , username : String
    , newMessageText: String
    , zone : Time.Zone
    , time : Time.Posix
    }

emptyUser : User
emptyUser =
    User "" "" "" Other "" "" 0 "" ""

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
              , Task.perform FetchMessages Time.now
              ]
  )



-- UPDATE
type Msg
    = NoOp
    | EntryChanged Model
    | SubmitMessage
    | DoNothing
    | HandleMessageSent (Result Http.Error (String.String))
    | SessionChanged Session
    | FetchMessages Time.Posix
    | HandleFetchedMessages (Result Http.Error (List Message))
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        EntryChanged updatedModel ->
            (updatedModel, Cmd.none)

        DoNothing ->
            (model, Cmd.none)

        SubmitMessage ->
            (model, sendMessage <| model)

        HandleMessageSent result ->
            case (Debug.log "response: "result) of
                Ok responseString ->
                    ( { model | content = (
                        [(Message model.newMessageText model.idYou 0 (toUtcString model.time model.zone))] ++
                        model.content
                        )
                        , newMessageText = ""
                      }
                    , Cmd.none)
                Err _ ->
                    (model , Cmd.none)

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
        [ Element.column [ width (px 600), height fill, spacing 10, padding 10, centerX, alignTop, explain Debug.todo ]
          <| List.reverse ((List.map (viewMessages model) model.content)) ++
              [ Element.row [ width (px 600), alignBottom, centerX]
                [ Input.button
                   [ paddingXY 10 15
                   , Border.rounded 4
                   , width fill
                   , Events.onClick DoNothing
                   ]
                   { onPress = Just SubmitMessage
                   , label = Input.multiline [width fill]
                             { text = model.newMessageText
                             , onChange = (\new -> EntryChanged { model | newMessageText = new })
                             , placeholder = Nothing
                             , label = Input.labelLeft [ Font.size 14, centerY ] (text "")
                             , spellcheck = True
                             }
                   }
                   , createButtonRight SubmitMessage "Create!"

                ]
              ]
        ]
    }


createButtonRight : Msg -> String -> Element Msg
createButtonRight msg caption =
    Input.button
        [ paddingXY 35 15
         , Background.color primaryColorL
         , Border.rounded 4
         , Border.width 1
         , Border.solid
         , fonts
         , Font.size 14
         , Font.semiBold
         , Font.color secondaryColor
         , mouseOver [ Font.color secondaryColorD ]
         , alignRight
         , centerY
         ]
        { onPress = Just msg, label = text (String.toUpper caption) }


viewMessages : Model -> Message -> Element Msg
viewMessages model message =
    el [ padding 10, width (fill |> maximum 255), Border.width 2, Border.rounded 20,
         (getPosition message.authorId model.idYou), Font.center
       ] (text message.body)



getPosition : Int -> Int -> Attribute msg
getPosition idMessage idYou =
    case (idYou == idMessage) of
        True ->
            Element.alignRight
        False ->
            Element.alignLeft

sendMessage : Model -> Cmd Msg
sendMessage model =
    case (String.isEmpty model.newMessageText) of
        False ->
            case model.session of
                Session.LoggedIn _ userInfo ->
                    Http.send HandleMessageSent
                        ( Api.postMessage
                          userInfo
                          (PostMessage 0 model.idYou (toUtcString model.time model.zone) model.newMessageText)
                          model.idFriend
                        )
                Session.Guest _ ->
                    Cmd.none
        True ->
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
    String.fromInt (Time.toDay zone time)
    ++ "T" ++
    String.fromInt (Time.toHour zone time)
    ++ ":" ++
    String.fromInt (Time.toMinute zone time)
    ++ ":" ++
    String.fromInt (Time.toSecond zone time)
    ++ "Z"


blue =
    Element.rgb 0.4 0.4 0.8

fonts =
    Font.family
        [ Font.typeface "-apple-system"
        , Font.typeface "BlinkMacSystemFont"
        , Font.typeface "Segoe UI"
        , Font.typeface "Roboto"
        , Font.typeface "Oxygen-Sans"
        , Font.typeface "Ubuntu"
        , Font.typeface "Cantarell"
        , Font.typeface "Helvetica Neue"
        , Font.sansSerif
        ]

primaryColorL =
    rgb255 255 255 255

secondaryColor =
    rgb255 96 125 139


secondaryColorD =
    rgb255 52 81 94
