module Main exposing (Model(..), Msg(..), ResponseString, blue, darkBlue, emptyCredentials, grey, handleErrorResponse, init, loginUser, main, mkWarning, noLabel, pure, red, responseToString, subscriptions, update, view, white)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Generated.DatingApi exposing (..)
import Http
import String


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model emptyCredentials Nothing, Cmd.none )


emptyCredentials : Credentials
emptyCredentials =
    Credentials "Bargsteen" "repsak"


type Model
    = Model Credentials ResponseString


type alias ResponseString =
    Maybe String


type Msg
    = Update Credentials
    | LoginClicked
    | HandleUserLogin (Result Http.Error String)


subscriptions userEntries =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model user response) =
    case msg of
        Update newFormEntries ->
            pure (Model newFormEntries response)

        LoginClicked ->
            ( Model user response, loginUser user )

        HandleUserLogin result ->
            case result of
                Ok token ->
                    pure (Model user (Just <| token))

                Err errResponse ->
                    handleErrorResponse errResponse user


handleErrorResponse : Http.Error -> Credentials -> ( Model, Cmd Msg )
handleErrorResponse errResponse user =
    case errResponse of
        Http.BadUrl url ->
            pure (Model user (Just <| "Bad url: " ++ url))

        Http.BadPayload _ _ ->
            pure (Model user (Just "bad payload"))

        Http.Timeout ->
            pure (Model user (Just "timeout"))

        Http.NetworkError ->
            pure (Model user (Just "networkerror"))

        Http.BadStatus statusResponse ->
            pure (Model user (Just <| "badstatus" ++ .body statusResponse))


loginUser : Credentials -> Cmd Msg
loginUser cred =
    Http.send HandleUserLogin <| postLogin cred


pure : Model -> ( Model, Cmd Msg )
pure userEntries =
    ( userEntries, Cmd.none )


view : Model -> Browser.Document Msg
view (Model userEntries response) =
    { title = "Login"
    , body =
        [ Element.layout
            [ Font.size 20
            ]
          <|
            Element.column [ width (px 800), height shrink, centerY, centerX, spacing 36, padding 10 ]
                [ el
                    [ Region.heading 1
                    , centerX
                    , Font.size 36
                    ]
                    (text "Login")
                , Input.username
                    [ spacing 12
                    ]
                    { text = userEntries.username
                    , placeholder = Just (Input.placeholder [] (text "Username"))
                    , onChange = \new -> Update { userEntries | username = new }
                    , label = Input.labelAbove [ Font.size 14 ] (text "Username")
                    }
                , Input.currentPassword [ spacing 12, width shrink ]
                    { text = userEntries.password
                    , placeholder = Just (Input.placeholder [] (text "Password"))
                    , onChange = \new -> Update { userEntries | password = new }
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
                    , label = Element.text "Login!"
                    }

                --, div [] [ text (String.reverse model.content) ]
                , Element.link [ Font.color blue ]
                    { url = "CreateUser.elm"
                    , label = text "Not yet a user? Click here to sign up."
                    }
                , Element.text <| responseToString response
                ]
        ]
    }


responseToString : ResponseString -> String
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
