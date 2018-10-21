module Page.Chat exposing (Model, Msg(..), init, subscriptions, update, view)
import Html exposing (Html)
import Skeleton
import Session as Session exposing (Session)
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


-- MODEL
type alias Model =
    { session : Session
    , title : String
    , content : List Message
    , idYou : Int
    , idFriend : Int
    , yourself : User
    , newMessageText: String
    }

emptyUser : User
emptyUser =
    User "" "" "" Other "" "" 0 "" ""

init : Session -> Int -> ( Model, Cmd Msg )
init session idFriend =
  ( Model (Debug.log "messages session:" session)
    "Messages"
    [(Message "User1" 5 "Hi"), (Message "User2" 6 "Hello"), (Message "User1" 5 "What's up?")]
    (Maybe.withDefault -1 (Session.getUserId session))
    idFriend
    emptyUser
    ""
  , (sendGetUser HandleGetUser (Maybe.withDefault -1 (Session.getUserId session)) session ))



-- UPDATE
type Msg
    = NoOp
    | EntryChanged Model
    | SubmitMessage
    | DoNothing
    | HandleMessageSent (Result Http.Error (String.String))
    | HandleGetUser (Result Http.Error (User))
    | SessionChanged Session


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

        HandleGetUser result ->
            case result of
                Ok fetchedUser ->
                    Debug.log (Debug.toString fetchedUser) ( { model | yourself = fetchedUser }, Cmd.none)

                Err errResponse ->
                    Debug.log (Debug.toString errResponse) ( { model | yourself = emptyUser }, Cmd.none )

        HandleMessageSent result ->
            case result of
                Ok responseString ->
                    ({ model | content = (model.content ++ [(Message model.yourself.userUsername model.idYou model.newMessageText)])
                     , newMessageText = "" }, Cmd.none)
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



-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Session.onChange SessionChanged (Session.getNavKey model.session)




-- VIEW
view : Model -> Skeleton.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        [ Element.column [ width (px 600), height fill, spacing 10, padding 10, explain Debug.todo ]
          <| (List.map (viewMessages model) model.content) ++
              [ Element.row [ width (px 600), height fill, alignBottom, centerX]
                [ Input.button
                   [ --Background.color red
                   --, Font.color white
                   --, Border.color darkBlue
                    paddingXY 10 15
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
         ]
        { onPress = Just msg, label = text (String.toUpper caption) }


viewMessages : Model -> Message -> Element Msg
viewMessages model message =
    el [ padding 10, width (fill |> maximum 255), Border.width 2, Border.rounded 20,
         (getPosition message.userId model.idYou), Font.center
       ] (text message.message)



getPosition : Int -> Int -> Attribute msg
getPosition idMessage idYou =
    case (idYou == idMessage) of
        True ->
            Element.alignRight
        False ->
            Element.alignLeft


sendGetUser : (Result Http.Error User -> msg) -> Int -> Session -> Cmd msg
sendGetUser responseMsg userId session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (Api.getUserById userId userInfo)
        Session.Guest _ ->
            Cmd.none

sendMessage : Model -> Cmd Msg
sendMessage model =
    case (String.isEmpty model.newMessageText) of
        False ->
            case model.session of
                Session.LoggedIn _ userInfo ->
                    Http.send HandleMessageSent (Api.postMessage userInfo (PostMessage 0 model.yourself.userUsername model.newMessageText) model.idFriend)
                Session.Guest _ ->
                    Cmd.none
        True ->
            Cmd.none

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
    