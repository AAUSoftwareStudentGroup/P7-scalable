module Session exposing (Session(..), PageType(..), Notification, Details, getNavKey, getUsername, getUserToken, addNotification, setNotifications, getNotifications, getNow, setNow, onChange, login, logout, createSessionFromLocalStorageValue)

import Browser.Navigation as Nav
import Html exposing (Html)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (..)
import Time
import Date exposing (Date)

import Api.Users
import Api.Authentication exposing (Token, UserInfo)
import Ports.UserInfoStoragePort as UserInfoPort

-- TYPES
type Session
    = LoggedIn Nav.Key (List Notification) Time.Posix UserInfo
    | Guest Nav.Key (List Notification) Time.Posix

type PageType msg
    = Scrollable (List (Html msg))
    | Fixed (List (Html msg))

type alias Notification =
    { body     : String
    , duration : Int
    , timeSet  : Time.Posix
    }

type alias Details msg =
    { title : String
    , session : Session
    , kids : PageType msg
    }


empty : Nav.Key -> Session
empty key =
    Guest key [] <| Time.millisToPosix 0

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

addNotification : Session -> String -> Session
addNotification session notificationBody =
    let
        notification = Notification notificationBody 3000 (getNow session)
    in
        case session of
            LoggedIn nav notifications now userInfo ->
                LoggedIn nav (notification :: notifications) now userInfo

            Guest nav notifications now ->
                Guest nav (notification :: notifications) now

setNotifications : Session -> List Notification -> Session
setNotifications session notifications =
    case session of
        LoggedIn nav _ now userInfo ->
            LoggedIn nav notifications now userInfo

        Guest nav _ now ->
            Guest nav notifications now

getNotifications : Session -> List Notification
getNotifications session =
    case session of
        LoggedIn _ notifications _ _ ->
            notifications

        Guest _ notifications _ ->
            notifications

getNow : Session -> Time.Posix
getNow session = 
    case session of
        LoggedIn _ _ now _ ->
            now
        Guest _ _ now ->
            now

setNow : Session -> Time.Posix -> Session
setNow session now = 
    case session of
        LoggedIn a b _ c ->
            LoggedIn a b now c
        Guest a b _ ->
            Guest a b now


login : UserInfo -> Cmd msg
login userInfo =
    UserInfoPort.storeUserInfo (Just userInfo)

logout : Cmd msg
logout =
    UserInfoPort.storeUserInfo Nothing

onChange : (Session -> msg) -> Nav.Key -> Sub msg
onChange toMsg key =
    UserInfoPort.onUserInfoChange (\value -> toMsg (createSessionFromLocalStorageValue value key))


-- HELPERS

createSessionFromLocalStorageValue : Maybe UserInfo -> Nav.Key -> Session
createSessionFromLocalStorageValue maybeValue key =
  case maybeValue of
      Nothing ->
        empty key

      Just userInfo ->
          LoggedIn key [] (Time.millisToPosix 0) userInfo
