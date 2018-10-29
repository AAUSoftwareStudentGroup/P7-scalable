port module Session exposing (Session(..), Details, getNavKey, getUserId, getUsername, onChange, login, logout, createSessionFromLocalStorageValue)

import Browser.Navigation as Nav
import Html exposing (Html)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (..)

--import DatingApi as Api exposing (User, UserInfo)
import Api.Types exposing (UserInfo)
import Api.Users

-- TYPES
type Session
    = LoggedIn Nav.Key UserInfo
    | Guest Nav.Key


type alias Details msg =
    { title : String
    , session : Session
    , kids : List (Html msg)
    }


empty : Nav.Key -> Session
empty key = Guest key

getNavKey : Session -> Nav.Key
getNavKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key

getUserInfo : Session -> Maybe UserInfo
getUserInfo session =
    case session of
        LoggedIn _ userInfo ->
            Just userInfo

        Guest _ ->
            Nothing

getUserId : Session -> Maybe Int
getUserId session =
    case session of
        LoggedIn _ userInfo ->
            Just userInfo.userId
        Guest _ ->
            Nothing


getUsername : Session -> Maybe String
getUsername session =
    case session of
        LoggedIn _ userInfo ->
            Just userInfo.username
        Guest _ ->
            Nothing

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
    onStoreChange (\value -> toMsg (Debug.log "Changed token" (createSessionFromLocalStorageValue value key)))


-- HELPERS

createSessionFromLocalStorageValue : Maybe Encode.Value -> Nav.Key -> Session
createSessionFromLocalStorageValue maybeValue key =
  case maybeValue of
      Nothing ->
        Guest key

      Just encodedSession ->
          case (decodeLocalStorageSession encodedSession) of
              Err _ ->
                  Guest key
              Ok token ->
                  LoggedIn key token


decodeLocalStorageSession : Encode.Value -> Result Decode.Error UserInfo
decodeLocalStorageSession val =
    Decode.decodeValue Decode.string val
      |> Result.andThen(\str -> Decode.decodeString Api.Users.decodeUserInfo str)


