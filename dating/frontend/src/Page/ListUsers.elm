module Page.ListUsers exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import String exposing (toUpper)
import String.Extra exposing (toSentenceCase)
import List exposing (map)
import Url

import DatingApi as Api exposing (User)
import Session exposing (Session, Details)
import Routing exposing (Route(..))
import UI.Elements as El



-- MODEL


type alias Model =
    { session : Session
    , title : String
    , users : List User
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session "List Users" []
    , sendGetUsers UsersFetched session
    )



-- UPDATE


type Msg
    = UsersFetched (Result Http.Error (List User))
    | SessionChanged Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersFetched result ->
            case result of
                Ok newUsers ->
                    ( { model | users = newUsers }, Cmd.none )

                Err error ->
                    Debug.log (Debug.toString error) ( { model | users = [] }, Cmd.none )
        SessionChanged session ->
            case session of
                Session.Guest key ->
                     ( { model | session = session }
                     , Routing.replaceUrl key (Routing.routeToString Home)
                     )
                Session.LoggedIn key _ ->
                  ( { model | session = session }
                  , Routing.replaceUrl key (Routing.routeToString ListUsers)
                  )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.onChange SessionChanged (Session.getNavKey model.session)



-- VIEW


view : Model -> Session.Details Msg
view model =
    { title = "All users"
    , session = model.session
    , kids =
        El.pageContent "All users" <|
            [ El.contentColumn 16 (map (showUser model.session) model.users) ]
    }




showUser : Session -> User -> Element Msg
showUser session user =
    El.userCard user.userUsername user.userId (Maybe.withDefault -1 (Session.getUserId session))


sendGetUsers : (Result Http.Error (List User) -> msg) -> Session -> Cmd msg
sendGetUsers responseMsg session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (Api.getUsers userInfo)
        Session.Guest _ ->
            Cmd.none
