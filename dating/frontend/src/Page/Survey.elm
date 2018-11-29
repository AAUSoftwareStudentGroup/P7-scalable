module Page.Survey exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (classList, src, title, style)
import Http
import Routing exposing (Route(..))
import Session exposing (Session, PageType(..))
import String
import Api.Questions exposing (Question, Answer)
import UI.Elements as El
import Html.Events as Events


-- MODEL


type alias Model =
    { session : Session
    , questions : List Question
    , currentQuestion : Question
    , answerValue : Int
    , loaded : Bool
    , response : Maybe String
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { session = session
      , questions = []
      , currentQuestion = Question "" ""
      , answerValue = 1
      , loaded = False
      , response = Nothing
      }
    , sendGetQuestions HandleQuestionsReceived session
    )


-- UPDATE


type Msg
    = HandleQuestionsReceived (Result Http.Error (List Question))
    | AnswerClicked Int
    | SubmitAnswer
    | HandleAnswerSubmitted (Result Http.Error (String.String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleQuestionsReceived result ->
            case result of
                Ok newQuestions ->
                    ( { model | questions = List.drop 1 newQuestions, currentQuestion = getFirstQuestion newQuestions, loaded = True }, Cmd.none )
                Err _ ->
                    ( { model | questions = [] }, Cmd.none )

        AnswerClicked newAnswer ->
            ( { model | answerValue = newAnswer }, Cmd.none )

        SubmitAnswer ->
            ( model, sendAnswer HandleAnswerSubmitted model.session <| createAnswer model.answerValue model.currentQuestion.id)

        HandleAnswerSubmitted result ->
            case result of
                Ok _ ->
                    if List.isEmpty model.questions then
                        ( { model | loaded = False }, sendGetQuestions HandleQuestionsReceived model.session)
                    else
                        ( { model | questions = List.drop 1 model.questions, currentQuestion = getFirstQuestion model.questions, answerValue = 1 }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Session.Details Msg
view model =
    { title = "Answer plz"
    , session = model.session
    , kids = Scrollable
        <| El.titledContentLoader model.loaded "Questions"
            ( if model.currentQuestion.id /= "" then
                [ El.textProperty "Question" model.currentQuestion.question
                , El.labelledRadio "Answer" AnswerClicked model.answerValue
                    [ ( "1", 1 )
                    , ( "2", 2 )
                    , ( "3", 3 )
                    , ( "4", 4 )
                    , ( "5", 5 )
                    ]
                , Html.button [Events.onClick SubmitAnswer] [Html.text "submit"]
                ]
                else
                 [ El.textProperty "Question" "no more questions" ]

            )
    }

getFirstQuestion : List Question -> Question
getFirstQuestion questions =
    case List.head questions of
        Just question -> question
        Nothing -> Question "" ""

sendGetQuestions : (Result Http.Error (List Question) -> msg) -> Session -> Cmd msg
sendGetQuestions responseMsg session =
    case session of
        Session.LoggedIn _ _ _ userInfo ->
            Http.send responseMsg (Api.Questions.getQuestions userInfo)
        Session.Guest _ _ _ ->
            Cmd.none

sendAnswer : (Result Http.Error (String.String) -> msg) -> Session -> Answer -> Cmd msg
sendAnswer responseMsg session answer =
    case session of
        Session.LoggedIn _ _ _ userInfo ->
            Http.send responseMsg (Api.Questions.postAnswer userInfo answer)
        Session.Guest _ _ _ ->
            Cmd.none

createAnswer : Int -> String -> Answer
createAnswer score id = Answer id score
