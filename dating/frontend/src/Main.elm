port module Main exposing (Model, Msg(..), Page(..), exit, init, main, route, stepCreateUser, stepMessages, stepUrl, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Debug
import Element exposing (..)
import Html exposing (Html)
import Page.CreateUser as CreateUser
import Page.ListUsers as ListUsers
import Page.Messages as Messages
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Login as Login exposing (subscriptions)
import Url
import Skeleton
import Session exposing (Session)
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)
import Url.Parser.Query as Query
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (decodeString, string)

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
    = NotFound Session
    | CreateUser CreateUser.Model
    | Login Login.Model
    | ListUsers ListUsers.Model
    | Messages Messages.Model
    | Profile Profile.Model


init : Maybe Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeValue url key =
    stepUrl url
        { key = key
        , page = NotFound (Session.createSessionFromLocalStorageValue maybeValue key)
        }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound session ->
            Skeleton.view never (NotFound.view session)

        CreateUser createUserModel ->
            Skeleton.view CreateUserMsg (CreateUser.view createUserModel)

        Login loginModel ->
            Skeleton.view LoginMsg (Login.view loginModel)

        Messages messagesModel ->
            Skeleton.view MessagesMsg (Messages.view messagesModel)

        ListUsers listUsersModel ->
            Skeleton.view ListUsersMsg (ListUsers.view listUsersModel)

        Profile profileModel ->
          Skeleton.view ProfileMsg (Profile.view profileModel)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        NotFound _ ->
            Sub.none

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


-- UPDATE


type Msg
  = NoOp
  | LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | CreateUserMsg CreateUser.Msg
  | ListUsersMsg ListUsers.Msg
  | LoginMsg Login.Msg
  | MessagesMsg Messages.Msg
  | ProfileMsg Profile.Msg

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
                CreateUser createUser ->
                    stepCreateUser model (CreateUser.update msg createUser)

                _ ->
                    ( model, Cmd.none )

        LoginMsg msg ->
            case model.page of
                Login login ->
                    stepLogin model (Login.update msg login)
                _ ->
                    ( model, Cmd.none )

        ListUsersMsg msg ->
            case model.page of
                ListUsers listUsers ->
                    stepListUsers model (ListUsers.update msg listUsers)
                _ -> ( model, Cmd.none )

        ProfileMsg msg ->
            case model.page of
                Profile profile ->
                    stepProfile model (Profile.update msg profile)
                _ -> ( model, Cmd.none )

        MessagesMsg msg ->
            case model.page of
                Messages messages ->
                    stepMessages model (Messages.update msg messages)
                _ -> ( model, Cmd.none )



stepCreateUser : Model -> ( CreateUser.Model, Cmd CreateUser.Msg ) -> ( Model, Cmd Msg )
stepCreateUser model ( createUser, cmds ) =
    ( { model | page = CreateUser createUser }
    , Cmd.map CreateUserMsg cmds
    )


stepLogin : Model -> ( Login.Model, Cmd Login.Msg ) -> ( Model, Cmd Msg )
stepLogin model ( login, cmds ) =
    ( { model | page = Login login }
    , Cmd.map LoginMsg cmds
    )


stepListUsers : Model -> ( ListUsers.Model, Cmd ListUsers.Msg ) -> ( Model, Cmd Msg )
stepListUsers model (listUsers, cmds) =
    ( { model | page = ListUsers listUsers}
    , Cmd.map ListUsersMsg cmds
    )

stepProfile : Model -> ( Profile.Model, Cmd Profile.Msg ) -> ( Model, Cmd Msg )
stepProfile model (profile, cmds) =
    ( { model | page = Profile profile }
    , Cmd.map ProfileMsg cmds
    )

stepMessages : Model -> ( Messages.Model, Cmd Messages.Msg ) -> ( Model, Cmd Msg )
stepMessages model ( messages, cmds ) =
    ( { model | page = Messages messages }
    , Cmd.map MessagesMsg cmds
    )

-- EXIT


exit : Model -> Session
exit model =
    case model.page of
        NotFound session ->
            session

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


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        session =
            exit model

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
                , route (s "user")
                    ( stepProfile model (Profile.init session 10))
                , route (s "messages")
                    (stepMessages model (Messages.init session))
                ]

    in
    case Parser.parse parser (Debug.log "queryToPathUrl:" queryToPathUrl) of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound session }
            , Cmd.none
            )

route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser
