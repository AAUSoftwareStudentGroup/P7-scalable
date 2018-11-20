module Page.ListUsers exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Navigation as Nav
import Browser.Dom as Dom exposing (Viewport)
import Time as Time
import Task as Task
import Html exposing (Html)
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Html.Keyed exposing (ul)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import String exposing (toUpper)
import String.Extra exposing (toSentenceCase)
import List exposing (map)
import Url

import Api.Users exposing (User)
import Session exposing (Session, PageType(..), Details)
import Routing exposing (Route(..))
import UI.Elements as El
import Ports.LoadMorePort exposing (LoadMoreData, loadMore)



-- MODEL

startPage = 0
usersPerPage = 12

type alias Model =
    { session   : Session
    , title     : String
    , loaded    : Bool
    , moreUsers : Bool
    , pageNum   : Int
    , users     : List User
    }


initModel : Session -> Model
initModel session
    = Model session "All users" False True (startPage + 1) []

init : Session -> ( Model, Cmd Msg )
init session =
    case session of
        Session.Guest _ _ ->
            ( initModel session
            , Routing.goToLogin (Session.getNavKey session)
            )
        Session.LoggedIn _ _ _ ->
            ( initModel session
            , sendGetUsers UsersFetched startPage session
            )


-- UPDATE


type Msg
    = UsersFetched (Result Http.Error (List User))
    | LoadMore LoadMoreData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersFetched result ->
            case result of
                Ok newUsers ->
                    ( { model | users = model.users ++ newUsers, loaded = True, moreUsers = (List.length newUsers == usersPerPage) }, Cmd.none )

                Err error ->
                    ( { model | users = [] }, Cmd.none )

        LoadMore _ ->
            ( { model | pageNum = model.pageNum + 1, loaded = False }
            , sendGetUsers UsersFetched model.pageNum model.session
            )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    loadMore LoadMore


-- VIEW


view : Model -> Session.Details Msg
view model =
    let
        myUsername = Maybe.withDefault "" (Session.getUsername model.session)
        allOtherUsers = List.filter (\user -> myUsername /= user.username) model.users
        bottomElement =
            if model.loaded then
                if model.moreUsers then
                    [ El.msgButtonFlat
                        []
                        (LoadMore True)
                        [ El.iconText "Load more" "keyboard_arrow_down" ]
                    ]
                else
                    [ Html.text "No more users" ]
            else
                El.loader

    in
        { title = "All users"
        , session = model.session
        , kids = Scrollable
            <| El.titledContent "All users"
                [ Html.ul
                    [ classList
                        [ ( "grid", True )
                        , ( "l-12", True )
                        , ( "s-12", True )
                        ]
                    ]
                    (List.map (showUser model.session) allOtherUsers)
                , Html.div
                    [ classList
                        [ ( "l-12", True )
                        , ( "s-12", True )
                        , ( "centered", True )
                        ]
                    ]
                    bottomElement
                ]
        }

showUser : Session -> User -> Html Msg
showUser session user =
    El.userCard user


sendGetUsers : (Result Http.Error (List User) -> msg) -> Int -> Session -> Cmd msg
sendGetUsers responseMsg pageNum session =
    case session of
        Session.LoggedIn _ _ userInfo ->
            Http.send responseMsg (Api.Users.getUsers pageNum usersPerPage userInfo)
        Session.Guest _ _ ->
            Cmd.none
