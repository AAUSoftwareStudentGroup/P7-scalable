port module Ports.LocalStoragePort exposing (..)

import Json.Encode as Encode

port storeLocally : Maybe Encode.Value -> Cmd msg

port onStoreChange : (Maybe Encode.Value -> msg) -> Sub msg
