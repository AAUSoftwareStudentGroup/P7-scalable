module Page.Messages exposing (Content(..), Messages, Model, Msg(..), blue, init, toText, update, view, viewContent)

import Html exposing (Html)
import Skeleton
import Session exposing (Session)
import Routing exposing (Route(..))

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


-- MODEL
type alias Model = 
    { session : Session
    , title : String
    , content : Content
    }


type Content
    = Content (List String)


type alias Messages =
    List String

init : Session -> ( Model, Cmd Msg )
init session =
  ( Model (Debug.log "messages session:" session) "Messages" (Content ["Message 1", "Message 2", "Message 3"])
  , Cmd.none
  )

-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Skeleton.Details msg
view model =
    { title = model.title
    , session = model.session
    , kids = [ viewContent model.title model.content ]
    }

viewContent : String -> Content -> Element msg
viewContent title (Content messages) =
      column [padding 20, spacing 20, Background.color blue] <| (List.map toText messages) ++
      [ link [] {url = (Routing.routeToString CreateUser), label = toText "To Create User"}] ++
      [ link [] {url = (Routing.routeToString ListUsers), label = toText "To all users"}]

toText : String -> Element msg
toText str =
    el [ Font.size 20 ] <| text str


blue =
    Element.rgb 0.4 0.4 0.8
