module Page.Messages exposing (Content(..), Messages, Model, Msg(..), blue, init, initialModel, toText, update, view, viewContent)

import Browser
import Element exposing (Element, column, el, layout, link, padding, row, spacing, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import GenHelpers exposing (Gender(..))
import Generated.DatingApi exposing (..)
import Html exposing (Html)
import Http
import Routing
import Skeleton
import String



-- MODEL


type alias Model =
    { title : String
    , content : Content
    }


type Content
    = Content (List String)


type alias Messages =
    List String


init : String -> String -> ( Model, Cmd Msg )
init title url =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    Model "Messages" (Content [ "Message 1", "Message 2", "Message 3" ])



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
    , kids = [ viewContent model.title model.content ]
    }


viewContent : String -> Content -> Html msg
viewContent title (Content messages) =
    layout [ Font.size 20 ] <|
        column [ padding 20, spacing 20, Background.color blue ] <|
            List.map toText messages
                ++ [ link [] { url = Routing.routeToString Routing.CreateUser, label = toText "To Create User" } ]


toText : String -> Element msg
toText str =
    el [ Font.size 20 ] <| text str


blue =
    Element.rgb 0.4 0.4 0.8
