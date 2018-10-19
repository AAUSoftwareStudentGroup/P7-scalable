module Page.Messages exposing (Content(..), Model, Msg(..), blue, init, toText, update, view, viewContent)

import Html exposing (Html)
import Skeleton
import Session
import Routing exposing (Route(..))
import Generated.DatingApi exposing (getMessages, Message)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http


-- MODEL
type alias Model = 
    { session : Session.Data
    , title : String
    , content : Content
    }


type Content
    = Content (List Message)


--type alias Messages =
--    List String

init : Session.Data -> ( Model, Cmd Msg )
init session =
  ( Model (Debug.log "messages session:" session) "Messages" (Content [(Message "User1" 5 "Hi"), (Message "User2" 6 "Hello"), (Message "User1" 5 "What's up?")])
  , (sendGetMessages HandleGetMessages "someAuthToken")
  )

-- UPDATE


type Msg
    = NoOp
    | HandleGetMessages (Result Http.Error (List Message))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        HandleGetMessages result ->
            case result of
                Ok fetchedMessages ->
                    Debug.log (Debug.toString fetchedMessages) ( {model | content = (Content fetchedMessages) }, Cmd.none)

                Err errResponse ->
                    Debug.log (Debug.toString errResponse) ( model, Cmd.none )



-- VIEW


view : Model -> Skeleton.Details msg
view model =
    { title = model.title
    , kids = [ viewContent model.title model.content ]
    }

viewContent : String -> Content -> Element msg
viewContent title (Content messages) =
      column [padding 20, spacing 20, Border.width 2] <| (List.map toText messages)

toText : Message -> Element msg
toText message =
    row [padding 20, spacing 20, Border.width 2, Background.color blue, width fill ]
    [ el [ Font.size 20, width fill ] <| text message.username
    , el [ Font.size 20, width fill, Background.color yellow ] <| text message.message
    ]


sendGetMessages : (Result Http.Error (List Message) -> msg) -> String -> Cmd msg
sendGetMessages responseMsg token =
    Http.send responseMsg (getMessages token)

blue =
    Element.rgb 0.4 0.4 0.8

yellow =
    Element.rgb 0.8 0.8 0.2
