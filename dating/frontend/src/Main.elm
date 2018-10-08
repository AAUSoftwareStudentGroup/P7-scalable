import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Element exposing (..)
import Page.CreateUser as CreateUser
import Page.Messages as Messages
import Page.NotFound as NotFound
import Url
import Url.Parser as Parser exposing (Parser, (</>), custom, fragment, map, oneOf, s, top)
import Skeleton
import Debug
import Session



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
  | Messages Messages.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
  stepUrl url
    { key = key
    , page = NotFound Session.empty
    }

-- VIEW

view : Model -> Browser.Document Msg
view model =
  case model.page of
    NotFound _ ->
      Skeleton.view never (NotFound.view)

    CreateUser createUser ->
      Skeleton.view never (CreateUser.view createUser)

    Messages unused ->
      Skeleton.view never (Messages.view unused)


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
  | MessagesMsg Messages.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case message of
    NoOp -> 
      (model, Cmd.none)

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
        CreateUser createUser -> stepCreateUser model (CreateUser.update msg createUser)
        _             -> ( model, Cmd.none )

    MessagesMsg msg ->
      case model.page of
        Messages messages -> stepMessages model (Messages.update msg messages)
        _             -> ( model, Cmd.none )


stepCreateUser : Model -> ( CreateUser.Model, Cmd CreateUser.Msg ) -> ( Model, Cmd Msg )
stepCreateUser model (createUser, cmds) = 
  ( { model | page = CreateUser createUser}
  , Cmd.map CreateUserMsg cmds
  )

stepMessages : Model -> ( Messages.Model, Cmd Messages.Msg ) -> ( Model, Cmd Msg )
stepMessages model (messages, cmds) =
  ( { model | page = Messages messages}
  , Cmd.map MessagesMsg cmds
  )


-- EXIT

exit : Model -> Session.Data
exit model =
  case model.page of
    NotFound session -> session
    CreateUser m -> m.session
    Messages m -> m.session


stepUrl : Url.Url -> Model -> (Model, Cmd Msg)
stepUrl url model =
  let
    session =
      exit model

    parser =
      s "src" </>
      oneOf
        [ route (s "Main.elm")
            ( stepCreateUser model (CreateUser.init session)
            )
        , route (s "create-user")
            ( stepCreateUser model (CreateUser.init session)
            )
        , route (s "messages")
            ( stepMessages model (Messages.init session)
            )
        ]
  in
  case Parser.parse parser (Debug.log "theURL" url) of
    Just answer ->
      Debug.log "parsedUrl" answer

    Nothing ->
      ( { model | page = NotFound session }
      , Cmd.none
      )

route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
  Parser.map handler parser