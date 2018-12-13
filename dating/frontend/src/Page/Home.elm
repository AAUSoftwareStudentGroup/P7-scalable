module Page.Home exposing (Model, init, view)

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
import UI.Elements as El



-- MODEL

startPage = 0
usersPerPage = 12

type alias Model =
    { session   : Session
    , title     : String
    , loaded    : Bool
    }


init : Session -> Model
init session = Model session "Home page" False

-- VIEW

view : Model -> Session.Details Never
view model =
    let
        text =
            case model.session of
                Session.LoggedIn _ _ _ userInfo ->
                    "Hello " ++ userInfo.username ++ ". Welcome back to Functional Dating!"
                Session.Guest _ _ _ ->
                    "Welcome to Functional Dating! Create a user to find love!"
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
                    [ Html.text text
                    , Html.br [] []
                    , Html.a [Attributes.href "/guide.pdf", Attributes.target "_blank"] [Html.text "Here is a guide on how to sign up and answer questions"]
                     ]
                ]
        }
