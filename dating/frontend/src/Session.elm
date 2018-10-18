port module Session exposing(..)

import Browser.Navigation as Nav
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (..)

-- TYPES

type alias Token
    = String

type Session
    = LoggedIn Nav.Key Token
    | Guest Nav.Key

empty : Nav.Key -> Session
empty key = Guest key

getNavKey : Session -> Nav.Key
getNavKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key

-- PERSISTENCE

port storeLocally : Maybe Encode.Value -> Cmd msg

login : Token -> Cmd msg
login token =
    storeLocally (Just (Encode.string token))


logout : Cmd msg
logout =
    storeLocally Nothing


port onStoreChange : (Maybe Encode.Value -> msg) -> Sub msg


createSessionFromLocalStorageValue : Maybe Encode.Value -> Nav.Key -> Session
createSessionFromLocalStorageValue maybeValue key =
  case maybeValue of
      Nothing ->
        Guest key

      Just encodedToken ->
          case (decodeToken encodedToken) of
              Err _ ->
                  Guest key
              Ok token ->
                  LoggedIn key token


decodeToken : Encode.Value -> Result Decode.Error Token
decodeToken val =
    Decode.decodeValue Decode.string val
      |> Result.andThen(\str -> Decode.decodeString tokenDecoder str)

tokenDecoder : Decoder Token
tokenDecoder =
    Decode.string


onChange : (Session -> msg) -> Nav.Key -> Sub msg
onChange toMsg key =
    onStoreChange (\value -> toMsg (Debug.log "Changed token" (createSessionFromLocalStorageValue value key)))
