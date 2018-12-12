port module Ports.LocalStoragePort exposing (..)

import Json.Encode as Encode

import Api.Authentication exposing (UserInfo)

port storeLocally : Maybe UserInfo -> Cmd msg

port onStoreChange : (Maybe UserInfo -> msg) -> Sub msg
