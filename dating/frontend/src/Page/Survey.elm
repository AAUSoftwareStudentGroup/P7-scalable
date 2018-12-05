module Page.Survey exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (Html, div)
import Html.Attributes as Attributes exposing (classList, class, src, title, style)
import Http
import Routing exposing (Route(..))
import Session exposing (Session, PageType(..))
import String
import Api.Questions exposing (Question, Answer, emptyQuestion)
import UI.Elements as El
import Html.Events as Events


-- MODEL

type Steps
    = Welcome
    | AnsweredTenFirst
    | AnsweringTenFirst
    | AnsweringMore

type alias Model =
    { session           : Session
    , step              : Steps
    , loaded            : Bool
    , questions         : List Question
    , currentQuestion   : Question
    , answerValue       : Int
    , response          : Maybe String
    }

emptyModel : Session -> Model
emptyModel session =
    Model session Welcome False [] emptyQuestion -1 Nothing

welcomeText : String
welcomeText =
    "We would like to know a bit about you in order to provide you with the best match possible. In order to do so, we ask you to please answer around 10 questions."

thankYouText : String
thankYouText =
    "Thank you very much, this should help us to find a good match for you. If you wish to answer more questions, feel free to do so. Otherwise you can do so from your profile page."

init : Session -> ( Model, Cmd Msg )
init session =
    case session of
        Session.Guest _ _ _ ->
            ( emptyModel session
            , Routing.goToLogin (Session.getNavKey session)
            )

        Session.LoggedIn _ _ _ userInfo ->
            let
                model = emptyModel session
            in
                if userInfo.firstLogIn then
                    ( model
                    , sendGetQuestions HandleQuestionsReceived session
                    )
                else
                    ( { model | step = AnsweringMore }
                    , sendGetQuestions HandleQuestionsReceived session
                    )


-- UPDATE

type Msg
    = WelcomeClicked
    | FinishedClicked
    | MoreClicked
    | HandleQuestionsReceived (Result Http.Error (List Question))
    | AnswerClicked Int
    | SubmitAnswer
    | HandleAnswerSubmitted (Result Http.Error (String.String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WelcomeClicked ->
            ( { model | step = AnsweringTenFirst }
            , Cmd.none
            )

        FinishedClicked ->
            ( model
            , Routing.replaceUrl (Session.getNavKey model.session) (Routing.routeToString ListUsers)
            )

        MoreClicked ->
            ( { model | step = AnsweringMore }
            , sendGetQuestions HandleQuestionsReceived model.session
            )

        HandleQuestionsReceived result ->
            case result of
                Ok newQuestions ->
                    ( nextQuestion model newQuestions
                    , Cmd.none
                    )

                Err errResponse ->
                    case errResponse of
                        Http.BadStatus response ->
                            if (response.status.code == 403) then
                                ( model, Session.logout )
                            else
                                ( { model | session = Session.addNotification model.session ("Error: " ++ .body response) }, Cmd.none )
                        _ ->
                            ( { model | session = Session.addNotification model.session "Error: Something went wrong" }, Cmd.none )

        AnswerClicked newAnswer ->
            ( { model | answerValue = newAnswer }
            , Cmd.none
            )

        SubmitAnswer ->
            if model.answerValue < 1 || model.answerValue > 5 then
                (model, Cmd.none)
            else
                ( model
                , sendAnswer HandleAnswerSubmitted model.session <| Answer model.currentQuestion.id model.answerValue
                )

        HandleAnswerSubmitted result ->
            case result of
                Ok _ ->
                    if List.isEmpty model.questions then
                        if model.step == AnsweringTenFirst then
                            ( { model | step = AnsweredTenFirst }
                            , Cmd.none
                            )
                        else
                            ( model
                            , sendGetQuestions HandleQuestionsReceived model.session
                            )

                    else
                        ( nextQuestion model model.questions
                        , Cmd.none
                        )

                Err errResponse ->
                    case errResponse of
                        Http.BadStatus response ->
                            if (response.status.code == 403) then
                                ( model, Session.logout )
                            else
                                ( { model | session = Session.addNotification model.session ("Error: " ++ .body response) }, Cmd.none )
                        _ ->
                            ( { model | session = Session.addNotification model.session "Error: Something went wrong" }, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Session.Details Msg
view model =
    { title = "Survey"
    , session = model.session
    , kids = Scrollable
        <| El.titledContentLoader model.loaded "Survey"
            <| case model.step of
                Welcome ->
                    let
                        username = Maybe.withDefault "" <| Session.getUsername model.session
                    in
                        [ El.modalMono ("Welcome to the site " ++ username ++ "!\n" ++ welcomeText) "Okay" WelcomeClicked ]
                AnsweredTenFirst ->
                    [ El.modalBinary thankYouText "Okay" FinishedClicked "More" MoreClicked ]
                _ ->
                    [ El.propertyGroup "Statement" model.currentQuestion.question
                    , El.labelledRadio "Answer - Strongly disagree to strongly agree" AnswerClicked model.answerValue
                        [ ( "1", 1 )
                        , ( "2", 2 )
                        , ( "3", 3 )
                        , ( "4", 4 )
                        , ( "5", 5 )
                        ]
                    , Html.button
                        [ Events.onClick SubmitAnswer
                        , classList
                            [ ( "btn", True)
                            , ( "l-12", True)
                            , ( "right", True) ]
                        ]
                        [ Html.text "Submit" ]
                    ]
    }

nextQuestion : Model -> List Question -> Model
nextQuestion model questionList =
    { model | questions = List.drop 1 questionList, currentQuestion = getFirstQuestion questionList, answerValue = -1, loaded = True }

getFirstQuestion : List Question -> Question
getFirstQuestion questions =
    case List.head questions of
        Just question ->
            question
        Nothing ->
            Question "" ""

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


