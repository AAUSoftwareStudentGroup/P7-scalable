module Page.Survey exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Http
import Routing exposing (Route(..))
import Session exposing (Session)
import Skeleton
import String



-- MODEL


type alias Model =
    { session : Session
    , questionId : Int
    , question : String
    , answer : Int
    , response : Maybe String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , questionId = 1
      , question = "How are you doing?"
      , answer = 3
      , response = Nothing
      }
    , Cmd.none
    )


getMockQuestion : Model -> Model
getMockQuestion model =
    { session = model.session
    , questionId = 2
    , question = "This is a new question?"
    , answer = 3
    , response = Nothing
    }



-- UPDATE


type Msg
    = TextChanged Model
    | AnswerClicked
      --| HandleAnswerClicked (Result Http.Error)
    | SessionChanged Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextChanged changedModel ->
            ( changedModel, Cmd.none )

        AnswerClicked ->
            ( getMockQuestion model, Cmd.none )

        --        HandleAnswerClicked result ->
        --            case result of
        --               Ok user ->
        --                    ( model, getNewQuestion )
        --
        --                Err errResponse ->
        --                    ( handleErrorResponse model errResponse, Cmd.none )
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
    { title = "Answer plz"
    , session = model.session
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
            (text "Quesion")
        , el
            [ spacing 12
            ]
            (text model.question)
        , Input.slider
            [ Element.height (Element.px 30)

            -- Here is where we're creating/styling the "track"
            , Element.behindContent
                (Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 2)
                    , Element.centerY
                    , Background.color grey
                    , Border.rounded 2
                    ]
                    Element.none
                )
            ]
            { onChange = \new -> TextChanged { model | answer = toInt new }
            , label = Input.labelAbove [] (text "Answer")
            , min = 1
            , max = 5
            , step = Just 1
            , value = toFloat model.answer
            , thumb =
                Input.defaultThumb
            }
        , Input.button
            [ Background.color red
            , Font.color white
            , Border.color darkBlue
            , paddingXY 32 16
            , Border.rounded 3
            , width fill
            ]
            { onPress = Just AnswerClicked
            , label = text "Submit!"
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


sendAnswer : Int -> Int -> Cmd Msg
sendAnswer qId uId =
    Cmd.none



--Http.send HandleUserLogin (Api.postLogin creds)


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
