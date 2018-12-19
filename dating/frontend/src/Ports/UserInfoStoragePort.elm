port module Ports.UserInfoStoragePort exposing (onUserInfoChange, storeUserInfo)

import Api.Authentication exposing (UserInfo)
import Json.Encode as Encode


port storeUserInfo : Maybe UserInfo -> Cmd msg


port onUserInfoChange : (Maybe UserInfo -> msg) -> Sub msg
