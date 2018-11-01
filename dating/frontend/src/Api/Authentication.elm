module Api.Authentication exposing (createAuthHeader, Credentials, credentialsEncoder, credentialsDecoder)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Http
import String
import Url

import Api.Types exposing (UserInfo)


type alias Credentials =
    { username : String
    , password : String
    }

credentialsEncoder : Credentials -> Encode.Value
credentialsEncoder x =
    Encode.object
        [ ( "CredentialDTOUsername", Encode.string x.username )
        , ( "CredentialDTOPassword", Encode.string x.password )
        ]


credentialsDecoder : Decoder Credentials
credentialsDecoder =
    Decode.succeed Credentials
        |> Pipeline.required "CredentialDTOUsername" Decode.string
        |> Pipeline.required "CredentialDTOPassword" Decode.string


createAuthHeader : UserInfo -> Http.Header
createAuthHeader userInfo =
  Http.header "Auth-Token" ("dating-auth-cookie=" ++ userInfo.authToken)