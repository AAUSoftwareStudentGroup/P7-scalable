module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Html exposing (Html)
import Url
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)
import Url.Parser.Query as Query
import Json.Encode as Encode
import Json.Decode as Decode
import Time as Time
import Http as Http

import Page.CreateUser as CreateUser
import Page.ListUsers as ListUsers
import Page.Messages as Messages
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Login as Login
import Page.Chat as Chat
import Url
import Session exposing (Session)
import DatingApi as DatingApi exposing (Message, getMessages)
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
    | ListUsers ListUsers.Model
    | Messages Messages.Model
    | Profile Profile.Model
    | Chat Chat.Model


init : Maybe Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeValue url key =
    stepUrl url
        { key = key
        , page = NotFound (NotFound.createModel (Session.createSessionFromLocalStorageValue maybeValue key))
        , numMessages = 0}



-- VIEW

view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound notFoundModel ->
            viewContent NotFoundMsg  (NotFound.view notFoundModel)

        CreateUser createUserModel ->
            viewContent CreateUserMsg (CreateUser.view createUserModel)

        Login loginModel ->
            viewContent LoginMsg (Login.view loginModel)

        Messages messagesModel ->
            viewContent MessagesMsg (Messages.view messagesModel)

        ListUsers listUsersModel ->
            viewContent ListUsersMsg (ListUsers.view listUsersModel)

        Chat chatModel ->
            viewContent ChatMsg (Chat.view chatModel)

        Profile profileModel ->
            viewContent ProfileMsg (Profile.view profileModel)

viewContent : (a -> msg) -> Session.Details a -> Browser.Document msg
viewContent toMsg details =
    { title = details.title
    , body = El.site toMsg details.kids details.session
    }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [case model.page of
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

        Chat chatModel ->
            Sub.map ChatMsg (Chat.subscriptions chatModel)

    , Time.every 1000 GetNumMessages ]


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
  | ChatMsg Chat.Msg
  | GetNumMessages Time.Posix
  | HandleGetMessages (Result Http.Error (List Message))

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
                _ -> ( model, Cmd.none )

        ProfileMsg msg ->
            case model.page of
                Profile profileModel ->
                    stepProfile model (Profile.update msg profileModel)
                _ -> ( model, Cmd.none )

        MessagesMsg msg ->
            case model.page of
                Messages messagesModel ->
                    stepMessages model (Messages.update msg messagesModel)
                _ -> ( model, Cmd.none )

        ChatMsg msg ->
            case model.page of
                Chat chat ->
                    stepChat model (Chat.update msg chat)
                _ -> ( model, Cmd.none )

        GetNumMessages newTime ->
            --(model, sendGetMessages HandleGetMessages (Debug.log "session: "(getSession model)))
            (model, Cmd.none)

        HandleGetMessages result ->
            case result of
                Ok fetchedMessages ->
                    Debug.log (Debug.toString fetchedMessages) ( {model | numMessages = (Debug.log "length: "(List.length fetchedMessages)) }, Cmd.none)

                Err errResponse ->
                    Debug.log (Debug.toString errResponse) ( model, Cmd.none )



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
stepListUsers model (listUsersModel, cmds) =
    ( { model | page = ListUsers listUsersModel}
    , Cmd.map ListUsersMsg cmds
    )

stepProfile : Model -> ( Profile.Model, Cmd Profile.Msg ) -> ( Model, Cmd Msg )
stepProfile model (profileModel, cmds) =
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

sendGetMessages : (Result Http.Error (List Message) -> msg) -> Session -> Cmd msg
sendGetMessages responseMsg session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (getMessages userInfo)
        Session.Guest _ ->
            Cmd.none


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

        ListUsers m ->
            m.session

        Profile m ->
            m.session

        Messages m ->
            m.session

        Chat m ->
            m.session


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        session =
            getSession model

        queryToPathUrl =
            { url | path = Maybe.withDefault "" url.query, query = Nothing}

        parser =
            s "path=" </>
            oneOf
                [ route (s "Main.elm")
                    (stepLogin model (Login.init session))
                , route (s "create-user")
                    (stepCreateUser model (CreateUser.init session))
                , route (s "login")
                    (stepLogin model (Login.init session))
                , route (s "list-users")
                    ( stepListUsers model (ListUsers.init session))
                , route (s "user" </> Parser.int)
                    (\id -> stepProfile model (Profile.init session (Debug.log "idParsed" id)))
                , route (s "messages")
                    (stepMessages model (Messages.init session))
                , route (s "chat" </> Parser.int)
                    (\idFriend -> stepChat model (Chat.init session idFriend))
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
