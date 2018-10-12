module Main exposing (Model, Msg(..), Page(..), exit, init, main, route, stepCreateUser, stepMessages, stepUrl, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Debug
import Element exposing (..)
import Html exposing (Html)
import Page.CreateUser as CreateUser
import Page.Login as Login
import Page.Messages as Messages
import Page.NotFound as NotFound
import Session
import Skeleton
import Url
import Url.Parser as Parser exposing ((</>), Parser, custom, fragment, map, oneOf, s, top)
import Url.Parser.Query as Query



-- MAIN


main : Program () Model Msg
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
    = NotFound Session.Data
    | CreateUser CreateUser.Model
    | Login Login.Model
    | Messages Messages.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    stepUrl url
        { key = key
        , page = NotFound (Session.empty key)
        }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound _ ->
            Skeleton.view never NotFound.view

        CreateUser createUser ->
            Skeleton.view CreateUserMsg (CreateUser.view createUser)

        Login login ->
            Skeleton.view LoginMsg (Login.view login)

        Messages unused ->
            Skeleton.view MessagesMsg (Messages.view unused)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UPDATE


type Msg
    = NoOp
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CreateUserMsg CreateUser.Msg
    | LoginMsg Login.Msg
    | MessagesMsg Messages.Msg


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

        LoginMsg msg ->
            case model.page of
                Login login ->
                    stepLogin model (Login.update msg login)
                _ ->
                    ( model, Cmd.none )
        CreateUserMsg msg ->
            case model.page of
                CreateUser createUser ->
                    stepCreateUser model (CreateUser.update msg createUser)

                _ ->
                    ( model, Cmd.none )

        MessagesMsg msg ->
            case model.page of
                Messages messages ->
                    stepMessages model (Messages.update msg messages)

                _ ->
                    ( model, Cmd.none )


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


stepMessages : Model -> ( Messages.Model, Cmd Messages.Msg ) -> ( Model, Cmd Msg )
stepMessages model ( messages, cmds ) =
    ( { model | page = Messages messages }
    , Cmd.map MessagesMsg cmds
    )



-- EXIT


exit : Model -> Session.Data
exit model =
    case model.page of
        NotFound session ->
            session

        CreateUser m ->
            m.session

        Messages m ->
            m.session

        Login m ->
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
