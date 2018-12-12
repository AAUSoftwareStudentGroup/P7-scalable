port module Ports.UserInfoStoragePort exposing (..)

import Json.Encode as Encode

import Api.Authentication exposing (UserInfo)

port storeUserInfo : Maybe UserInfo -> Cmd msg

port onUserInfoChange : (Maybe UserInfo -> msg) -> Sub msg
