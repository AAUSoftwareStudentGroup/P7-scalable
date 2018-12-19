module Page.NotFound exposing (Content(..), Messages, Model, init, view)

import Html exposing (Html, div)
import Routing exposing (..)
import Session exposing (Details, PageType(..), Session)



-- MODEL


type alias Model =
    { session : Session
    , title   : String
    , content : Content
    }


type Content
    = Content String


type alias Messages =
    List String


init : Session -> Model
init session =
    Model session "Page Not Found Title" (Content <| "Not found..")



-- VIEW


view : Model -> Session.Details Never
view model =
    { title = model.title
    , session = model.session
    , kids =
        Scrollable <|
            [ Html.text "This page was not found" ]
    }
