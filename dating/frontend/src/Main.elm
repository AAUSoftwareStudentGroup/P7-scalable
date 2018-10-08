import Browser
import Browser.Navigation as Nav
import Html exposing (Html)
import Element exposing (..)
import Page.CreateUser as CreateUser
import Page.ListUsers as ListUsers
import Page.Messages as Messages
import Page.NotFound as NotFound
import Page.Profile as Profile
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
    | ListUsers ListUsers.Model
    | Messages Messages.Model
    | Profile Profile.Model


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

    ListUsers listUsers ->
        Skeleton.view never (ListUsers.view listUsers)

    Messages unused ->
      Skeleton.view never (Messages.view unused)

    Profile profile ->
      Skeleton.view never (Profile.view profile)


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
  | ListUsersMsg ListUsers.Msg
  | MessagesMsg Messages.Msg
  | ProfileMsg Profile.Msg

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

    ListUsersMsg msg ->
      case model.page of
        ListUsers listUsers -> stepListUsers model (ListUsers.update msg listUsers)
        _             -> ( model, Cmd.none )

    MessagesMsg msg ->
      case model.page of
        Messages messages -> stepMessages model (Messages.update msg messages)
        _             -> ( model, Cmd.none )

    ProfileMsg msg ->
      case model.page of
        Profile profile -> stepProfile model (Profile.update msg profile)
        _             -> ( model, Cmd.none )


stepCreateUser : Model -> ( CreateUser.Model, Cmd CreateUser.Msg ) -> ( Model, Cmd Msg )
stepCreateUser model (createUser, cmds) = 
  ( { model | page = CreateUser createUser}
  , Cmd.map CreateUserMsg cmds
  )

stepListUsers : Model -> ( ListUsers.Model, Cmd ListUsers.Msg ) -> ( Model, Cmd Msg )
stepListUsers model (listUsers, cmds) =
    ( { model | page = ListUsers listUsers}
    , Cmd.map ListUsersMsg cmds
    )

stepMessages : Model -> ( Messages.Model, Cmd Messages.Msg ) -> ( Model, Cmd Msg )
stepMessages model (messages, cmds) =
  ( { model | page = Messages messages}
  , Cmd.map MessagesMsg cmds
  )

stepProfile : Model -> ( Profile.Model, Cmd Profile.Msg ) -> ( Model, Cmd Msg )
stepProfile model (profile, cmds) =
    ( { model | page = Profile profile }
    , Cmd.map ProfileMsg cmds
    )
-- EXIT

exit : Model -> Session.Data
exit model =
  case model.page of
    NotFound session -> session
    CreateUser m -> m.session
    ListUsers m -> m.session
    Messages m -> m.session
    Profile m -> m.session


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
        , route (s "list-users")
            ( stepListUsers model (ListUsers.init session)
            )
        , route (s "messages")
            ( stepMessages model (Messages.init session)
            )
        , route (s "user")
            ( stepProfile model (Profile.init session)
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
