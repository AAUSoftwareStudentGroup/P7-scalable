module Page.ListUsers exposing (..)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Url
import Http
import Json.Decode as Decode exposing (field, Decoder, int, string, list)
import String.Extra exposing (toSentenceCase)
import Generated.DatingApi exposing (User, getUsers)
import Skeleton

import Debug


-- MODEL

type alias Model =
    {
    users : List User
    }

initialModel : Model
initialModel = Model []

init : () -> ( Model, Cmd Msg )
init flags =
  ( Model [], sendGetUsers UsersFetched "ndygwfzqobfwjhzsxnghpgclvccgdtlprrdyllffkmijhdjikqugizmtpxyvppqb")


-- UPDATE

type Msg
  = UsersFetched(Result Http.Error (List User))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UsersFetched result ->
         case result of
            Ok newUsers ->
                ( { model | users = newUsers }, Cmd.none)
            Err error ->
                Debug.log (Debug.toString error) ( { model | users = [] }, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- VIEW

view : Model -> Skeleton.Details msg
view model =
  { title = "All users"
  , kids =
        [ Element.layout
            [ Font.size 20
            ]
          <|
            Element.column [ width (px 800), height shrink, centerY, centerX, spacing 36, padding 10 ]
                ((el
                    [ Region.heading 1
                    , centerX
                    , Font.size 36
                    ]
                    (text "Users")
                ) :: (List.map showUser model.users))
        ]
    }


showUser : User -> Element Msg
showUser user =
    Element.column [centerX, spacing 10] [
        el [centerX, Font.size 24] (text (toSentenceCase user.userUsername))
        , createLink "View profile" ("user/" ++ user.userUsername)
    ]

createLink : String -> String -> Element Msg
createLink label path =
    Element.link [centerX] { label = text label, url = path }

sendGetUsers : (Result Http.Error (List User) -> msg) -> String -> Cmd msg
sendGetUsers responseMsg userToken =
    Http.send responseMsg (getUsers userToken)
