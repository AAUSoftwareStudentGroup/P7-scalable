module Page.NotFound exposing (Content(..), Messages, Model, createModel, Msg(..), init, subscriptions, update, view)

import Html exposing (Html, div)
import Routing exposing (..)
import Session exposing (Session, Details)



-- MODEL


type alias Model =
    { session : Session
    , title : String
    , content : Content
    }


type Content
    = Content String


type alias Messages =
    List String


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session "Page Not Found Title" (Content <| "Not found..")
    , Cmd.none
    )

createModel : Session -> Model
createModel session =
    Model session "Page Not Found Title" (Content <| "Not found..")


-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        [ Html.text "This page was not found" ]
    }
