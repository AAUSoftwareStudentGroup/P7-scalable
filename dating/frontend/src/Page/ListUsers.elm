module Page.ListUsers exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attributes exposing (classList)
import Html.Keyed exposing (ul)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import String exposing (toUpper)
import String.Extra exposing (toSentenceCase)
import List exposing (map)
import Url

import Api.Users exposing (User)
import Session exposing (Session, Details)
import Routing exposing (Route(..))
import UI.Elements as El



-- MODEL


type alias Model =
    { session   : Session
    , title     : String
    , loaded    : Bool
    , users     : List User
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session "List Users" False []
    , sendGetUsers UsersFetched session
    )


-- UPDATE


type Msg
    = UsersFetched (Result Http.Error (List User))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersFetched result ->
            case result of
                Ok newUsers ->
                    ( { model | users = newUsers, loaded = True }, Cmd.none )

                Err error ->
                    Debug.log (Debug.toString error) ( { model | users = [] }, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Session.Details Msg
view model =
    let
        myUsername = Maybe.withDefault "" (Session.getUsername model.session)
    in
        { title = "All users"
        , session = model.session
        , kids =
            El.titledContentLoader model.loaded "All users"
                [ Html.ul
                    [ classList
                            [ ( "grid", True )
                            , ( "l-12", True )
                            , ( "s-12", True )
                            ]

                    ]
                    (List.map (showUser2 model.session) <| List.filter (\user -> myUsername /= user.username) model.users)
                ]
        }

showUser2 : Session -> User -> Html Msg
showUser2 session user =
    El.userCard user


showUser : Session -> User -> (String, Html Msg)
showUser session user =
    ( user.username
    , El.userCard user
    )


sendGetUsers : (Result Http.Error (List User) -> msg) -> Session -> Cmd msg
sendGetUsers responseMsg session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (Api.Users.getUsers userInfo)
        Session.Guest _ ->
            Cmd.none
