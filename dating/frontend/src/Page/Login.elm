module Page.Login exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Events as Events
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Http
import String

import DatingApi as Api exposing (User, Credentials)
import Session exposing (Session, Details)
import Routing exposing (Route(..))


-- MODEL


type alias Model =
    { session : Session
    , title : String
    , username : String
    , password : String
    , response : Maybe String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , title = "Login"
      , username = "Bargsteen"
      , password = "repsak"
      , response = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = TextChanged Model
    | LoginClicked
    | HandleUserLogin (Result Http.Error User)
    | SessionChanged Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextChanged changedModel ->
            ( changedModel, Cmd.none )

        LoginClicked ->
            ( model, sendLogin (Credentials model.username model.password) )

        HandleUserLogin result ->
            case result of
                Ok user ->
                    ( model, Session.login user )

                Err errResponse ->
                    ( handleErrorResponse model errResponse, Cmd.none )

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


view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        [ viewContent model ]
    }


viewContent : Model -> Element Msg
viewContent model =
        column [ width (px 800), height shrink, centerY, centerX, alignTop, spacing 36, padding 10 ]
            [ el
                [ Region.heading 1
                , centerX
                , Font.size 36
                ]
                (text "Login")
            , Input.username
                [ spacing 12
                ]
                { text = model.username
                , placeholder = Just (Input.placeholder [] (text "Username"))
                , onChange = \new -> TextChanged { model | username = new }
                , label = Input.labelAbove [ Font.size 14 ] (text "Username")
                }
            , Input.currentPassword [ spacing 12, width shrink ]
                { text = model.password
                , placeholder = Just (Input.placeholder [] (text "Password"))
                , onChange = \new -> TextChanged { model | password = new }
                , label = Input.labelAbove [ Font.size 14 ] (text "Password")
                , show = False
                }
            , Input.button
                [ Background.color red
                , Font.color white
                , Border.color darkBlue
                , paddingXY 32 16
                , Border.rounded 3
                , width fill
                ]
                { onPress = Just LoginClicked
                , label = text "Login!"
                }
            , link [ Font.color blue ]
                { url = "create-user"
                , label = text "Not yet a user? Click here to sign up."
                }
            , el [] (text (responseToString model.response))
            ]


handleErrorResponse : Model -> Http.Error -> Model
handleErrorResponse model errResponse =
    case errResponse of
        Http.BadUrl url ->
            { model | response = Just <| "Bad url: " ++ url }

        Http.BadPayload _ _ ->
            { model | response = Just "Bad payload" }

        Http.Timeout ->
            { model | response = Just "Timeout" }

        Http.NetworkError ->
            { model | response = Just "Networkerror" }

        Http.BadStatus statusResponse ->
            { model | response = Just <| "Badstatus" ++ .body statusResponse }



sendLogin : Credentials -> Cmd Msg
sendLogin creds =
    Http.send HandleUserLogin (Api.postLogin creds)


responseToString : Maybe String -> String
responseToString r =
    case r of
        Just msg ->
            msg

        Nothing ->
            ""


mkWarning warning =
    el
        [ Font.color red
        , Font.size 14
        , alignRight
        , moveDown 6
        ]
        (text warning)


noLabel =
    Input.labelAbove [] none


white =
    Element.rgb 1 1 1


grey =
    Element.rgb 0.9 0.9 0.9


blue =
    Element.rgb 0 0 0.8


red =
    Element.rgb 0.8 0 0


darkBlue =
    Element.rgb 0 0 0.9
