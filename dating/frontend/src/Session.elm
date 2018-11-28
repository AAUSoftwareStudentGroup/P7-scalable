port module Session exposing (Session(..), PageType(..), Notification, Details, getNavKey, getUsername, getUserToken, addNotification, getNotifications, getNow, setNow, onChange, login, logout, createSessionFromLocalStorageValue)

import Browser.Navigation as Nav
import Html exposing (Html)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (..)
import Date exposing (Date)

import Api.Users
import Api.Authentication exposing (Token, UserInfo)

-- TYPES
type Session
    = LoggedIn Nav.Key (List Notification) Date UserInfo
    | Guest Nav.Key (List Notification) Date

type PageType msg
    = Scrollable (List (Html msg))
    | Fixed (List (Html msg))

type alias Notification = String

type alias Details msg =
    { title : String
    , session : Session
    , kids : PageType msg
    }


empty : Nav.Key -> Session
empty key = Guest key [] (Date.fromOrdinalDate 0 1)

getNavKey : Session -> Nav.Key
getNavKey session =
    case session of
        LoggedIn key _ _ _ ->
            key

        Guest key _ _ ->
            key

getUserInfo : Session -> Maybe UserInfo
getUserInfo session =
    case session of
        LoggedIn _ _ _ userInfo ->
            Just userInfo

        Guest _ _ _ ->
            Nothing


getUsername : Session -> Maybe String
getUsername session =
    case session of
        LoggedIn _ _ _ userInfo ->
            Just userInfo.username
        Guest _ _ _ ->
            Nothing

getUserToken : Session -> Token
getUserToken session =
    case session of
        LoggedIn _ _ _ userInfo ->
            userInfo.authToken

        Guest _ _ _ ->
            ""

addNotification : Session -> Notification -> Session
addNotification session notification =
    case session of
        LoggedIn nav notifications now userInfo ->
            LoggedIn nav (notification :: notifications) now userInfo

        Guest nav notifications now ->
            Guest nav (notification :: notifications) now 

getNotifications : Session -> List Notification
getNotifications session =
    case session of
        LoggedIn _ notifications _ _ ->
            notifications

        Guest _ notifications _ ->
            notifications

getNow : Session -> Date
getNow session = 
    case session of
        LoggedIn _ _ now _ ->
            now
        Guest _ _ now ->
            now

setNow : Session -> Date -> Session
setNow session now = 
    case session of
        LoggedIn a b _ c ->
            LoggedIn a b now c
        Guest a b _ ->
            Guest a b now

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
        empty key

      Just encodedSession ->
          case (decodeLocalStorageSession encodedSession) of
              Err _ ->
                  empty key
              Ok token ->
                  LoggedIn key [] (Date.fromOrdinalDate 0 1) token


decodeLocalStorageSession : Encode.Value -> Result Decode.Error UserInfo
decodeLocalStorageSession val =
    Decode.decodeValue Decode.string val
      |> Result.andThen(\str -> Decode.decodeString Api.Users.decodeUserInfo str)
