module Page.ListUsers exposing (Model, Msg(..), init, subscriptions, update, view)

import Api.Users exposing (Match, User)
import Browser.Dom as Dom exposing (Viewport)
import Browser.Navigation as Nav
import Date exposing (Date)
import Html exposing (Html)
import Html.Attributes as Attributes exposing (classList)
import Html.Events as Events
import Html.Keyed exposing (ul)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import List exposing (map)
import Ports.LoadMorePort exposing (LoadMoreData, loadMore)
import Routing exposing (Route(..))
import Session exposing (Details, PageType(..), Session)
import String exposing (toUpper)
import String.Extra exposing (toSentenceCase)
import Task as Task
import Time as Time
import UI.Elements as El
import Url



-- MODEL


startPage =
    0


usersPerPage =
    12


type alias Model =
    { session : Session
    , title : String
    , loaded : Bool
    , moreUsers : Bool
    , pageNum : Int
    , users : List Match
    }


initModel : Session -> Model
initModel session =
    Model session "Matches" False True (startPage + 1) []


init : Session -> ( Model, Cmd Msg )
init session =
    case session of
        Session.Guest _ _ _ ->
            ( initModel session
            , Routing.goToLogin (Session.getNavKey session)
            )

        Session.LoggedIn _ _ _ _ ->
            ( initModel session
            , sendGetUsers UsersFetched startPage session
            )



-- UPDATE


type Msg
    = UsersFetched (Result Http.Error (List Match))
    | LoadMore LoadMoreData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersFetched result ->
            case result of
                Ok newUsers ->
                    let
                        userList =
                            List.reverse <| List.sortBy .score <| (model.users ++ newUsers)
                    in
                    ( { model | users = userList, loaded = True, moreUsers = List.length newUsers == usersPerPage }, Cmd.none )

                Err error ->
                    case error of
                        Http.BadStatus response ->
                            if response.status.code == 403 then
                                ( model, Session.logout )

                            else
                                ( { model | session = Session.addNotification model.session ("Error: " ++ .body response) }, Cmd.none )

                        _ ->
                            ( { model | session = Session.addNotification model.session "Error: Something went wrong" }, Cmd.none )

        LoadMore _ ->
            let
                command =
                    if model.moreUsers then
                        sendGetUsers UsersFetched model.pageNum model.session

                    else
                        Cmd.none
            in
            ( { model | pageNum = model.pageNum + 1, loaded = not <| model.moreUsers }
            , command
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    loadMore LoadMore



-- VIEW


view : Model -> Session.Details Msg
view model =
    let
        myUsername =
            Maybe.withDefault "" (Session.getUsername model.session)

        allOtherUsers =
            List.filter (\user -> myUsername /= user.user.username) model.users

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
    { title = "Matches"
    , session = model.session
    , kids =
        Scrollable <|
            El.titledContent "Matches"
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


showUser : Session -> Match -> Html Msg
showUser session match =
    let
        age =
            Api.Users.getAge match.user <| Session.getNow session
    in
    El.userCard match age


sendGetUsers : (Result Http.Error (List Match) -> msg) -> Int -> Session -> Cmd msg
sendGetUsers responseMsg pageNum session =
    case session of
        Session.LoggedIn _ _ _ userInfo ->
            Http.send responseMsg (Api.Users.getMatches userInfo pageNum usersPerPage)

        Session.Guest _ _ _ ->
            Cmd.none
