module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Html exposing (Html)
import Url
import Url.Parser as Parser exposing ((</>), Parser, top)
import Url.Parser.Query as Query
import Json.Encode as Encode
import Json.Decode as Decode
import Time as Time
import Http as Http

import Page.CreateUser as CreateUser
import Page.Home as Home
import Page.ListUsers as ListUsers
import Page.Login as Login exposing (subscriptions)
import Page.Messages as Messages
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Survey as Survey
import Page.Login as Login
import Page.Logout as Logout
import Page.Chat as Chat
import Url
import Session exposing (Session)
import Api.Messages exposing (ConversationPreviewDTO)
import Routing as Routing
import UI.Elements as El



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
    , numMessages : Int
    }


type Page
    = NotFound NotFound.Model
    | CreateUser CreateUser.Model
    | Login Login.Model
    | Logout Logout.Model
    | Home Home.Model
    | ListUsers ListUsers.Model
    | Messages Messages.Model
    | Profile Profile.Model
    | Chat Chat.Model
    | Survey Survey.Model


init : Maybe Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeValue url key =
    if String.startsWith "path=" (Maybe.withDefault "" url.query) then
        (
            { key = key
            , page = NotFound (NotFound.createModel (Session.createSessionFromLocalStorageValue maybeValue key))
            , numMessages = 0
            },
            Routing.replaceUrl key (String.dropLeft 5 (Maybe.withDefault "" url.query))
        )
    else
        stepUrl url
            { key = key
            , page = NotFound (NotFound.createModel (Session.createSessionFromLocalStorageValue maybeValue key))
            , numMessages = 0
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

        Logout logoutModel ->
            viewContent LogoutMsg (Logout.view logoutModel)

        Home homeModel ->
            viewContent HomeMsg (Home.view homeModel)

        Messages messagesModel ->
            viewContent MessagesMsg (Messages.view messagesModel)

        ListUsers listUsersModel ->
            viewContent ListUsersMsg (ListUsers.view listUsersModel)

        Chat chatModel ->
            viewContent ChatMsg (Chat.view chatModel)

        Profile profileModel ->
            viewContent ProfileMsg (Profile.view profileModel)

        Survey surveyModel ->
            viewContent SurveyMsg (Survey.view surveyModel)


viewContent : (a -> msg) -> Session.Details a -> Browser.Document msg
viewContent toMsg details =
    { title = details.title
    , body = El.site toMsg details.kids details.session
    }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
        case model.page of
            NotFound notFoundModel ->
                Sub.map NotFoundMsg (NotFound.subscriptions notFoundModel)

            CreateUser createUserModel ->
                Sub.map CreateUserMsg (CreateUser.subscriptions createUserModel)

            Login loginModel ->
                Sub.map LoginMsg (Login.subscriptions loginModel)

            Logout logoutModel ->
                Sub.map LogoutMsg (Logout.subscriptions logoutModel)

            Home homeModel ->
                Sub.map HomeMsg (Home.subscriptions homeModel)

            Messages messagesModel ->
                Sub.map MessagesMsg (Messages.subscriptions messagesModel)

            ListUsers listUsersModel ->
                Sub.map ListUsersMsg (ListUsers.subscriptions listUsersModel)

            Profile profileModel ->
                Sub.map ProfileMsg (Profile.subscriptions profileModel)

            Chat chatModel ->
                Sub.map ChatMsg (Chat.subscriptions chatModel)

            Survey surveyModel ->
                Sub.map SurveyMsg (Survey.subscriptions surveyModel)

        , Session.onChange SessionChanged (Session.getNavKey (getSession model))

      --, Time.every 1000 GetNumMessages
    ]





-- UPDATE


type Msg
  = NoOp
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | NotFoundMsg NotFound.Msg
  | CreateUserMsg CreateUser.Msg
  | HomeMsg Home.Msg
  | ListUsersMsg ListUsers.Msg
  | LoginMsg Login.Msg
  | LogoutMsg Logout.Msg
  | MessagesMsg Messages.Msg
  | ProfileMsg Profile.Msg
  | ChatMsg Chat.Msg
  | SurveyMsg Survey.Msg
  | SessionChanged Session
  | LogOutClicked
  | GetNumMessages Time.Posix
  | HandleGetMessages (Result Http.Error (List ConversationPreviewDTO))


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

        LogoutMsg msg ->
            case model.page of
                Logout logoutModel ->
                    stepLogout model (Logout.update msg logoutModel)
                _ ->
                    ( model, Cmd.none )

        HomeMsg msg ->
            case model.page of
                Home homeModel ->
                    stepHome model (Home.update msg homeModel)
                _ -> ( model, Cmd.none )

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

        ChatMsg msg ->
            case model.page of
                Chat chat ->
                    stepChat model (Chat.update msg chat)
                _ -> ( model, Cmd.none )

        SurveyMsg msg ->
            case model.page of
                Survey surveyModel ->
                    stepSurvey model (Survey.update msg surveyModel)
                _ -> ( model, Cmd.none )

        SessionChanged session ->
            case session of
                Session.Guest key _ ->
                    ( { model | page = (replacePage model.page session) }
                    , Routing.replaceUrl key (Routing.routeToString Routing.Home)
                    )
                Session.LoggedIn key _ _ ->
                    ( { model | page = (replacePage model.page session) }
                    , Routing.replaceUrl key (Routing.routeToString Routing.ListUsers)
                    )

        LogOutClicked ->
            ( model, Session.logout )

        GetNumMessages newTime ->
            --(model, sendGetMessages HandleGetMessages (Debug.log "session: "(getSession model)))
            (model, Cmd.none)

        HandleGetMessages result ->
            case result of
                Ok fetchedMessages ->
                    ( {model | numMessages = List.length fetchedMessages }, Cmd.none)

                Err errResponse ->
                    ( model, Cmd.none )



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

        Chat m ->
             Chat { m | session = session }

        Survey m ->
             Survey { m | session = session }



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

stepLogout : Model -> ( Logout.Model, Cmd Logout.Msg ) -> ( Model, Cmd Msg )
stepLogout model ( logoutModel, cmds ) =
    ( { model | page = Logout logoutModel }
    , Cmd.map LogoutMsg cmds
    )

stepHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
stepHome model (homeModel, cmds) =
    ( { model | page = Home homeModel}
    , Cmd.map HomeMsg cmds
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

stepChat : Model -> ( Chat.Model, Cmd Chat.Msg ) -> ( Model, Cmd Msg )
stepChat model ( chat, cmds ) =
    ( { model | page = Chat chat }
    , Cmd.map ChatMsg cmds
    )

sendGetMessages : (Result Http.Error (List ConversationPreviewDTO) -> msg) -> Session -> Cmd msg
sendGetMessages responseMsg session =
    case session of
        Session.LoggedIn _ _ userInfo ->
            Http.send responseMsg (Api.Messages.getConvoPreview userInfo)
        Session.Guest _ _ ->
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

        Chat m ->
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
                , route (Parser.s "list-users")
                    (stepListUsers model (ListUsers.init session))
                , route (Parser.s "user" </> Parser.string)
                    (\username -> stepProfile model (Profile.init session username))
                , route (Parser.s "messages")
                    (stepMessages model (Messages.init session))
                , route (Parser.s "chat" </> Parser.string)
                    (\username -> stepChat model (Chat.init session username))
                , route (Parser.s "survey")
                    (stepSurvey model (Survey.init session))
                ]

    in
        case Parser.parse parser {url | path = Maybe.withDefault url.path (Url.percentDecode url.path)} of
            Just answer ->
                answer

            Nothing ->
                ( { model | page = NotFound (NotFound.createModel session) }
                , Cmd.none
                )

route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser
