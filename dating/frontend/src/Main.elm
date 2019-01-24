module Main exposing (Model, Msg(..), Page(..), getSession, init, main, removeOldNotifications, replacePage, route, sendGetMessages, stepCreateUser, stepEditUser, stepHome, stepListUsers, stepLogin, stepLogout, stepMessages, stepNotFound, stepProfile, stepSurvey, stepUrl, subscriptions, update, view, viewContent)

import Api.Authentication exposing (UserInfo)
import Api.Messages exposing (ConversationPreview)
import Browser
import Browser.Navigation as Nav
import Date exposing (Date)
import Element exposing (..)
import Html exposing (Html)
import Http as Http
import Json.Decode as Decode
import Json.Encode as Encode
import Page.CreateUser as CreateUser
import Page.EditUser as EditUser
import Page.Home as Home
import Page.ListUsers as ListUsers
import Page.Login as Login
import Page.Logout as Logout
import Page.Messages as Messages
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Survey as Survey
import Routing as Routing
import Session exposing (Session)
import Task exposing (Task)
import Time as Time
import UI.Elements as El
import Url
import Url.Parser as Parser exposing ((</>), Parser, top)
import Url.Parser.Query as Query



-- MAIN


main : Program (Maybe UserInfo) Model Msg
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
    | Logout Logout.Model
    | Home Home.Model
    | ListUsers ListUsers.Model
    | EditUser EditUser.Model
    | Messages Messages.Model
    | Profile Profile.Model
    | Survey Survey.Model


init : Maybe UserInfo -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeUserInfo url key =
    ( Model key (NotFound <| NotFound.init <| Session.createSessionFromLocalStorageValue maybeUserInfo key)
    , Routing.replaceUrl key <| String.dropLeft 5 <| Maybe.withDefault "" url.query
    )



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CreateUserMsg CreateUser.Msg
    | ListUsersMsg ListUsers.Msg
    | LoginMsg Login.Msg
    | LogoutMsg Logout.Msg
    | MessagesMsg Messages.Msg
    | EditUserMsg EditUser.Msg
    | ProfileMsg Profile.Msg
    | SurveyMsg Survey.Msg
    | SessionChanged Session
    | LogOutClicked
    | RemoveOldNotifications Time.Posix
    | GetTimeNow Time.Posix


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

        LogoutMsg msg ->
            case model.page of
                Logout logoutModel ->
                    stepLogout model (Logout.update msg logoutModel)

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

        EditUserMsg msg ->
            case model.page of
                EditUser editUserModel ->
                    stepEditUser model (EditUser.update msg editUserModel)

                _ ->
                    ( model, Cmd.none )

        SurveyMsg msg ->
            case model.page of
                Survey surveyModel ->
                    stepSurvey model (Survey.update msg surveyModel)

                _ ->
                    ( model, Cmd.none )

        SessionChanged session ->
            case session of
                Session.Guest key _ _ ->
                    ( { model | page = replacePage model.page session }
                    , Routing.replaceUrl key (Routing.routeToString Routing.Home)
                    )

                Session.LoggedIn key _ _ userInfo ->
                    if userInfo.firstLogIn then
                        ( { model | page = replacePage model.page session }
                        , Routing.replaceUrl key (Routing.routeToString Routing.Survey)
                        )

                    else
                        ( { model | page = replacePage model.page session }
                        , Routing.replaceUrl key (Routing.routeToString Routing.ListUsers)
                        )

        LogOutClicked ->
            ( model, Session.logout )

        RemoveOldNotifications newTime ->
            let
                newSession =
                    removeOldNotifications newTime (getSession model)
            in
            ( { model | page = replacePage model.page newSession }, Cmd.none )

        GetTimeNow now ->
            let
                newSession =
                    Session.setNow (getSession model) now
            in
            ( { model | page = replacePage model.page newSession }, Cmd.none )


removeOldNotifications : Time.Posix -> Session -> Session
removeOldNotifications now session =
    let
        nowMillis =
            Time.posixToMillis now

        remainingNotifications =
            List.filter (\notification -> (nowMillis - Time.posixToMillis notification.timeSet) < notification.duration) <| Session.getNotifications session
    in
    Session.setNotifications session remainingNotifications


replacePage : Page -> Session -> Page
replacePage page session =
    case page of
        NotFound m ->
            NotFound { m | session = session }

        CreateUser m ->
            CreateUser { m | session = session }

        Login m ->
            Login { m | session = session }

        Logout m ->
            Logout { m | session = session }

        Home m ->
            Home { m | session = session }

        ListUsers m ->
            ListUsers { m | session = session }

        Profile m ->
            Profile { m | session = session }

        Messages m ->
            Messages { m | session = session }

        EditUser m ->
            EditUser { m | session = session }

        Survey m ->
            Survey { m | session = session }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.page of
            CreateUser createUserModel ->
                Sub.map CreateUserMsg (CreateUser.subscriptions createUserModel)

            Messages messagesModel ->
                Sub.map MessagesMsg (Messages.subscriptions messagesModel)

            EditUser editUserModel ->
                Sub.map EditUserMsg (EditUser.subscriptions editUserModel)

            ListUsers listUsersModel ->
                Sub.map ListUsersMsg (ListUsers.subscriptions listUsersModel)

            _ ->
                Sub.none

        -- This case handles all the pages without subscriptions
        , Session.onChange SessionChanged (Session.getNavKey (getSession model))
        , Time.every 1000 GetTimeNow
        , Time.every 1000 RemoveOldNotifications
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound notFoundModel ->
            viewContent never (NotFound.view notFoundModel)

        CreateUser createUserModel ->
            viewContent CreateUserMsg (CreateUser.view createUserModel)

        Login loginModel ->
            viewContent LoginMsg (Login.view loginModel)

        Logout logoutModel ->
            viewContent LogoutMsg (Logout.view logoutModel)

        Home homeModel ->
            viewContent never (Home.view homeModel)

        Messages messagesModel ->
            viewContent MessagesMsg (Messages.view messagesModel)

        EditUser editUserModel ->
            viewContent EditUserMsg (EditUser.view editUserModel)

        ListUsers listUsersModel ->
            viewContent ListUsersMsg (ListUsers.view listUsersModel)

        Profile profileModel ->
            viewContent ProfileMsg (Profile.view profileModel)

        Survey surveyModel ->
            viewContent SurveyMsg (Survey.view surveyModel)


viewContent : (a -> msg) -> Session.Details a -> Browser.Document msg
viewContent toMsg details =
    { title = details.title
    , body = El.site details toMsg
    }


stepNotFound : Model -> NotFound.Model -> ( Model, Cmd Msg )
stepNotFound model notFoundModel =
    ( { model | page = NotFound notFoundModel }
    , Cmd.none
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


stepLogout : Model -> ( Logout.Model, Cmd Logout.Msg ) -> ( Model, Cmd Msg )
stepLogout model ( logoutModel, cmds ) =
    ( { model | page = Logout logoutModel }
    , Cmd.map LogoutMsg cmds
    )


stepHome : Model -> Home.Model -> ( Model, Cmd Msg )
stepHome model homeModel =
    ( { model | page = Home homeModel }
    , Cmd.none
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


stepEditUser : Model -> ( EditUser.Model, Cmd EditUser.Msg ) -> ( Model, Cmd Msg )
stepEditUser model ( editUserModel, cmds ) =
    ( { model | page = EditUser editUserModel }
    , Cmd.map EditUserMsg cmds
    )


sendGetMessages : (Result Http.Error (List ConversationPreview) -> msg) -> Session -> Cmd msg
sendGetMessages responseMsg session =
    case session of
        Session.LoggedIn _ _ _ userInfo ->
            Http.send responseMsg (Api.Messages.getConvoPreview userInfo)

        Session.Guest _ _ _ ->
            Cmd.none


stepSurvey : Model -> ( Survey.Model, Cmd Survey.Msg ) -> ( Model, Cmd Msg )
stepSurvey model ( surveyModel, cmds ) =
    ( { model | page = Survey surveyModel }
    , Cmd.map SurveyMsg cmds
    )



-- SESSION


getSession : Model -> Session
getSession model =
    case model.page of
        NotFound m ->
            m.session

        CreateUser m ->
            m.session

        Login m ->
            m.session

        Logout m ->
            m.session

        Home m ->
            m.session

        ListUsers m ->
            m.session

        Profile m ->
            m.session

        Messages m ->
            m.session

        EditUser m ->
            m.session

        Survey m ->
            m.session


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        session =
            getSession model

        parser =
            Parser.oneOf
                [ route top
                    (stepHome model (Home.init session))
                , route (Parser.s "create-user")
                    (stepCreateUser model (CreateUser.init session))
                , route (Parser.s "login")
                    (stepLogin model (Login.init session))
                , route (Parser.s "logout")
                    (stepLogout model (Logout.init session))
                , route (Parser.s "matches")
                    (stepListUsers model (ListUsers.init session))
                , route (Parser.s "user" </> Parser.string)
                    (\username -> stepProfile model (Profile.init session username))
                , route (Parser.s "messages")
                    (stepMessages model (Messages.init session Nothing))
                , route (Parser.s "messages" </> Parser.string)
                    (\username -> stepMessages model (Messages.init session (Just username)))
                , route (Parser.s "edit")
                    (stepEditUser model (EditUser.init session))
                , route (Parser.s "survey")
                    (stepSurvey model (Survey.init session))
                ]
    in
    case Parser.parse parser { url | path = Maybe.withDefault url.path (Url.percentDecode url.path) } of
        Just ( m, c ) ->
            ( m
            , Cmd.batch
                [ Task.perform GetTimeNow Time.now
                , c
                ]
            )

        Nothing ->
            ( { model | page = NotFound (NotFound.init session) }
            , Cmd.none
            )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser
