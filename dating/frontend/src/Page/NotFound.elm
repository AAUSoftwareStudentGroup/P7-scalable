module Page.NotFound exposing (..)

import Html exposing (Html)
import Session exposing (Session)
import Skeleton

import Element exposing (..)
import Element.Font as Font


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


-- UPDATE

type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> (model, Cmd.none)


-- VIEW

view : Session -> Skeleton.Details msg
view session =
    { title = "Not found!"
    , session = session
    , kids = [ viewContent "Not Found" (Content "This page was not found.") ]
    }


viewContent : String -> Content -> Element msg
viewContent title (Content message) =
    toText message

toText : String -> Element msg
toText str = el [Font.size 20] <| text str 
