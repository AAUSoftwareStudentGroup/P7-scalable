module Page.NotFound exposing (Content(..), Messages, Model, createModel, Msg(..), init, subscriptions, toText, update, view, viewContent)

import Element exposing (..)
import Element.Font as Font
import Html exposing (Html)
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
    | SessionChanged Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
    { title = model.title
    , session = model.session
    , kids = [ viewContent "Not Found" (Content "This page was not found.") ]
    }


viewContent : String -> Content -> Element msg
viewContent title (Content message) =
    toText message


toText : String -> Element msg
toText str =
    el [ Font.size 20 ] <| text str
