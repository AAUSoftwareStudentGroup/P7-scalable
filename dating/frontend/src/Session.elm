port module Session exposing (Session(..), Notification, Details, getNavKey, getUsername, getUserToken, addNotification, getNotifications, onChange, login, logout, createSessionFromLocalStorageValue)

import Browser.Navigation as Nav
import Html exposing (Html)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (..)

import Api.Users
import Api.Authentication exposing (Token, UserInfo)

-- TYPES
type Session
    = LoggedIn Nav.Key (List Notification) UserInfo
    | Guest Nav.Key (List Notification)


type alias Notification = String

type alias Details msg =
    { title : String
    , session : Session
    , kids : List (Html msg)
    }


empty : Nav.Key -> Session
empty key = Guest key []

getNavKey : Session -> Nav.Key
getNavKey session =
    case session of
        LoggedIn key _ _ ->
            key

        Guest key _ ->
            key

getUserInfo : Session -> Maybe UserInfo
getUserInfo session =
    case session of
        LoggedIn _ _ userInfo ->
            Just userInfo

        Guest _ _ ->
            Nothing


getUsername : Session -> Maybe String
getUsername session =
    case session of
        LoggedIn _ _ userInfo ->
            Just userInfo.username
        Guest _ _ ->
            Nothing

getUserToken : Session -> Token
getUserToken session =
    case session of
        LoggedIn _ _ userInfo ->
            userInfo.authToken

        Guest _ _ ->
            ""

addNotification : Session -> Notification -> Session
addNotification session notification =
    case session of
        LoggedIn nav notifications userInfo ->
            LoggedIn nav (notification :: notifications) userInfo

        Guest nav notifications ->
            Guest nav (notification :: notifications)

getNotifications : Session -> List Notification
getNotifications session =
    case session of
        LoggedIn _ notifications _ ->
            notifications

        Guest _ notifications ->
            notifications

-- PERSISTENCE

port storeLocally : Maybe Encode.Value -> Cmd msg

login : UserInfo -> Cmd msg
login userInfo =
    storeLocally (Just (Api.Users.encodeUserInfo userInfo))


logout : Cmd msg
logout =
    storeLocally Nothing


port onStoreChange : (Maybe Encode.Value -> msg) -> Sub msg

onChange : (Session -> msg) -> Nav.Key -> Sub msg
onChange toMsg key =
    onStoreChange (\value -> toMsg (createSessionFromLocalStorageValue value key))


-- HELPERS

createSessionFromLocalStorageValue : Maybe Encode.Value -> Nav.Key -> Session
createSessionFromLocalStorageValue maybeValue key =
  case maybeValue of
      Nothing ->
        Guest key []

      Just encodedSession ->
          case (decodeLocalStorageSession encodedSession) of
              Err _ ->
                  Guest key []
              Ok token ->
                  LoggedIn key [] token


decodeLocalStorageSession : Encode.Value -> Result Decode.Error UserInfo
decodeLocalStorageSession val =
    Decode.decodeValue Decode.string val
      |> Result.andThen(\str -> Decode.decodeString Api.Users.decodeUserInfo str)
