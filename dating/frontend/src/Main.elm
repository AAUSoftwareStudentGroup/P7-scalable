module Main exposing (Model, Msg(..), Page(..), exit, init, main, route, stepCreateUser, stepListUsers, stepLogin, stepMessages, stepNotFound, stepProfile, stepUrl, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Html exposing (Html)
import Json.Decode as Decode
import Json.Encode as Encode
import Page.CreateUser as CreateUser
import Page.ListUsers as ListUsers
import Page.Login as Login exposing (subscriptions)
import Page.Messages as Messages
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Survey as Survey
import Session exposing (Session)
import Skeleton
import UI.Elements as El
import Url
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)
import Url.Parser.Query as Query



-- MAIN


main : Program (Maybe Encode.Value) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound NotFound.Model
    | CreateUser CreateUser.Model
    | Login Login.Model
    | ListUsers ListUsers.Model
    | Messages Messages.Model
    | Profile Profile.Model
    | Survey Survey.Model


init : Maybe Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeValue url key =
    stepUrl url
        { key = key
        , page = NotFound (NotFound.createModel (Session.createSessionFromLocalStorageValue maybeValue key))
        }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound notFoundModel ->
            viewContent NotFoundMsg (NotFound.view notFoundModel)

        CreateUser createUserModel ->
            viewContent CreateUserMsg (CreateUser.view createUserModel)

        Login loginModel ->
            viewContent LoginMsg (Login.view loginModel)

        Messages messagesModel ->
            viewContent MessagesMsg (Messages.view messagesModel)

        ListUsers listUsersModel ->
            viewContent ListUsersMsg (ListUsers.view listUsersModel)

        Profile profileModel ->
            Skeleton.view ProfileMsg (Profile.view profileModel)

        Survey surveyModel ->
            Skeleton.view SurveyMsg (Survey.view surveyModel)


viewContent : (a -> msg) -> Session.Details a -> Browser.Document msg
viewContent toMsg details =
    { title = details.title
    , body = El.site toMsg details.kids details.session
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        NotFound notFoundModel ->
            Sub.map NotFoundMsg (NotFound.subscriptions notFoundModel)

        CreateUser createUserModel ->
            Sub.map CreateUserMsg (CreateUser.subscriptions createUserModel)

        Login loginModel ->
            Sub.map LoginMsg (Login.subscriptions loginModel)

        Messages messagesModel ->
            Sub.map MessagesMsg (Messages.subscriptions messagesModel)

        ListUsers listUsersModel ->
            Sub.map ListUsersMsg (ListUsers.subscriptions listUsersModel)

        Profile profileModel ->
            Sub.map ProfileMsg (Profile.subscriptions profileModel)

        Survey surveyModel ->
            Sub.map SurveyMsg (Survey.subscriptions surveyModel)



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NotFoundMsg NotFound.Msg
    | CreateUserMsg CreateUser.Msg
    | ListUsersMsg ListUsers.Msg
    | LoginMsg Login.Msg
    | MessagesMsg Messages.Msg
    | ProfileMsg Profile.Msg
    | SurveyMsg Survey.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Nav.load href
                    )

        UrlChanged url ->
            stepUrl url model

        NotFoundMsg msg ->
            case model.page of
                NotFound notFoundModel ->
                    stepNotFound model (NotFound.update msg notFoundModel)

                _ ->
                    ( model, Cmd.none )

        CreateUserMsg msg ->
            case model.page of
                CreateUser createUserModel ->
                    stepCreateUser model (CreateUser.update msg createUserModel)

                _ ->
                    ( model, Cmd.none )

        LoginMsg msg ->
            case model.page of
                Login loginModel ->
                    stepLogin model (Login.update msg loginModel)

                _ ->
                    ( model, Cmd.none )

        ListUsersMsg msg ->
            case model.page of
                ListUsers listUsersModel ->
                    stepListUsers model (ListUsers.update msg listUsersModel)

                _ ->
                    ( model, Cmd.none )

        ProfileMsg msg ->
            case model.page of
                Profile profileModel ->
                    stepProfile model (Profile.update msg profileModel)

                _ ->
                    ( model, Cmd.none )

        MessagesMsg msg ->
            case model.page of
                Messages messagesModel ->
                    stepMessages model (Messages.update msg messagesModel)

                _ ->
                    ( model, Cmd.none )

        SurveyMsg msg ->
            case model.page of
                Survey surveyModel ->
                    stepSurvey model (Survey.update msg surveyModel)

                _ ->
                    ( model, Cmd.none )


stepNotFound : Model -> ( NotFound.Model, Cmd NotFound.Msg ) -> ( Model, Cmd Msg )
stepNotFound model ( notFoundModel, cmds ) =
    ( { model | page = NotFound notFoundModel }
    , Cmd.map NotFoundMsg cmds
    )


stepCreateUser : Model -> ( CreateUser.Model, Cmd CreateUser.Msg ) -> ( Model, Cmd Msg )
stepCreateUser model ( createUserModel, cmds ) =
    ( { model | page = CreateUser createUserModel }
    , Cmd.map CreateUserMsg cmds
    )


stepLogin : Model -> ( Login.Model, Cmd Login.Msg ) -> ( Model, Cmd Msg )
stepLogin model ( loginModel, cmds ) =
    ( { model | page = Login loginModel }
    , Cmd.map LoginMsg cmds
    )


stepListUsers : Model -> ( ListUsers.Model, Cmd ListUsers.Msg ) -> ( Model, Cmd Msg )
stepListUsers model ( listUsersModel, cmds ) =
    ( { model | page = ListUsers listUsersModel }
    , Cmd.map ListUsersMsg cmds
    )


stepProfile : Model -> ( Profile.Model, Cmd Profile.Msg ) -> ( Model, Cmd Msg )
stepProfile model ( profileModel, cmds ) =
    ( { model | page = Profile profileModel }
    , Cmd.map ProfileMsg cmds
    )


stepMessages : Model -> ( Messages.Model, Cmd Messages.Msg ) -> ( Model, Cmd Msg )
stepMessages model ( messagesModel, cmds ) =
    ( { model | page = Messages messagesModel }
    , Cmd.map MessagesMsg cmds
    )


stepSurvey : Model -> ( Survey.Model, Cmd Survey.Msg ) -> ( Model, Cmd Msg )
stepSurvey model ( surveyModel, cmds ) =
    ( { model | page = Survey surveyModel }
    , Cmd.map SurveyMsg cmds
    )



-- EXIT


exit : Model -> Session
exit model =
    case model.page of
        NotFound m ->
            m.session

        CreateUser m ->
            m.session

        Login m ->
            m.session

        ListUsers m ->
            m.session

        Profile m ->
            m.session

        Messages m ->
            m.session

        Survey m ->
            m.session


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        session =
            exit model

        queryToPathUrl =
            { url | path = Maybe.withDefault ("path=" ++ url.path) url.query, query = Nothing }

        parser =
            s "path="
                </> oneOf
                        [ route (s "Main.elm")
                            (stepLogin model (Login.init session))
                        , route (s "create-user")
                            (stepCreateUser model (CreateUser.init session))
                        , route (s "login")
                            (stepLogin model (Login.init session))
                        , route (s "list-users")
                            (stepListUsers model (ListUsers.init session))
                        , route (s "user" </> Parser.int)
                            (\id -> stepProfile model (Profile.init session (Debug.log "idParsed" id)))
                        , route (s "messages")
                            (stepMessages model (Messages.init session))
                        , route (s "survey")
                            (stepSurvey model (Survey.init session))
                        ]
    in
    case Parser.parse parser (Debug.log "queryToPathUrl:" queryToPathUrl) of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound (NotFound.createModel session) }
            , Cmd.none
            )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser
