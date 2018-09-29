import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Html exposing (Html)
import Url
import Http
import Json.Decode as Decode exposing (field, Decoder, int, string, list)
import UserApi exposing (..)

main : Program () Model Msg
main =
    Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , users : List User
    }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( Model key url [], getUsers )


-- UPDATE


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | UsersFetched(Result Http.Error (List User))
  | FetchUsers


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.load (Url.toString(url)) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )

    UsersFetched result ->
         case result of
            Ok newUsers ->
                ( { model | users = newUsers }, Cmd.none)
            Err _ ->
                ( { model | users = [] }, Cmd.none)

    FetchUsers ->
        (model, getUsers)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model = { title = "URL Interceptor"
    , body = [
        Element.layout [] <| Element.column [] (List.map showUser model.users)
    ]
    }

showUser : User -> Element Msg
showUser user =
    row [alignTop] [
        {-image UserPicture [width (px 60), height (px 60)] {caption = "", src = user.url},-}

        el [] ( text (user.userUsername ++ "    ") )
        , viewLink ("/chat/" ++ user.userUsername)
    ]

viewLink : String -> Element Msg
viewLink path =
    Element.link [] { label = text path, url = path }


getUsers : Cmd Msg
getUsers =
    Http.send UsersFetched (UserApi.getUsersRequest)

{-
getUsers : Cmd Msg
getUsers =
    let
        url = "https://jsonplaceholder.typicode.com/users"
    in
        Http.send UsersFetched (Http.get url decodeUsers)


decodeUsers : Decode.Decoder (List User)
decodeUsers = Decode.list decodeUser


decodeUser : Decode.Decoder User
decodeUser = Decode.map6 User
    (field "userUsername" string)
    (field "userEmail" string)
    (field "userProfileText" string)
    (field "userGender" string)
    (field "userBirthday" string)
    (field "userTown" string)



decodeUser : Decode.Decoder User
decodeUser = Decode.map6 User
    (field "username" string)
    (field "email" string)
    (field "website" string)
    (field "email" string)
    (field "email" string)
    (field "email" string)
-}