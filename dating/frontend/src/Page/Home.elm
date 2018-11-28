module Page.Home exposing (Model, Msg(..), init, subscriptions, update, view)

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
    }


initModel : Session -> Model
initModel session
    = Model session "Home page" False

init : Session -> ( Model, Cmd Msg )
init session =
    case session of
        Session.Guest _ _ _ ->
            ( initModel session
            , Cmd.none
            )
        Session.LoggedIn _ _ _ _ ->
            ( initModel session
            , Cmd.none
            )


-- UPDATE


type Msg
    = Home


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Home -> (model, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW


view : Model -> Session.Details Msg
view model =
    let
        text =
            case model.session of
                Session.LoggedIn _ _ _ userInfo ->
                    userInfo.username
                Session.Guest _ _ _ ->
                    "Welcome to Functional Dating"
    in
        { title = "Home"
        , session = model.session
        , kids = Scrollable
            <| El.titledContent "Home"
                [ Html.div
                    [ classList
                        [ ( "l-12", True )
                        , ( "s-12", True )
                        , ( "centered", True )
                        ]
                    ]
                    [ Html.text text ]
                ]
        }
