port module Page.Login exposing (Model, Msg, init, view, update, decodeToken)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Generated.DatingApi exposing (..)
import Html exposing (Html)
import Http
import Session
import Skeleton
import String
import Routing exposing (replaceUrl, Route(..))
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode


-- MODEL


type alias Model =
    { session : Session.Data
    , title : String
    , username : String
    , password : String
    , response : Maybe String
    }


init : Session.Data -> ( Model, Cmd Msg )
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
    | HandleUserLogin (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextChanged changedModel ->
            ( changedModel, Cmd.none )

        LoginClicked ->
            ( model, loginCmd model.username model.password )

        HandleUserLogin result ->
            case result of
                Ok token ->
                    ( Debug.log "tokenIsSet" { model | session = Session.LoggedIn (Session.navKey model.session) token }
                    , Cmd.batch [
                     storeTokenInCache token
                    , Routing.replaceUrl (Session.navKey model.session) (Routing.routeToString ListUsers )] )

                Err errResponse ->
                    ( handleErrorResponse model errResponse, Cmd.none )



-- VIEW


view : Model -> Skeleton.Details Msg
view model =
    { title = model.title
    , kids =
        [ viewContent model ]
    }


viewContent : Model -> Element Msg
viewContent model =
        column [ width (px 800), height shrink, centerY, centerX, spacing 36, padding 10 ]
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


loginCmd : String -> String -> Cmd Msg
loginCmd username password =
    Http.send HandleUserLogin <| postLogin (Credentials username password)


type alias Token = String


--port onStoreChange : (Value -> msg) -> Sub msg
port storeCache : Maybe Value -> Cmd msg


storeTokenInCache : String -> Cmd msg
storeTokenInCache token =
  storeCache (Just (Json.Encode.string token))


logout : Cmd msg
logout =
    storeCache Nothing

tokenDecoder : Decoder Token
tokenDecoder =
    Decode.string

decodeToken : Value -> Result Decode.Error Token
decodeToken val = Decode.decodeValue tokenDecoder val


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
