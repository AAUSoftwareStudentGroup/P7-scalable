module Api.Authentication exposing (Credentials, Token, UserInfo, createAuthHeader, credentialsDecoder, credentialsEncoder)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import String
import Url


type alias Token =
    String


type alias UserInfo =
    { authToken : Token
    , username : String
    , firstLogIn : Bool
    }


type alias Credentials =
    { username : String
    , password : String
    }


credentialsEncoder : Credentials -> Encode.Value
credentialsEncoder x =
    Encode.object
        [ ( "username", Encode.string x.username )
        , ( "password", Encode.string x.password )
        ]


credentialsDecoder : Decoder Credentials
credentialsDecoder =
    Decode.succeed Credentials
        |> Pipeline.required "username" Decode.string
        |> Pipeline.required "password" Decode.string


createAuthHeader : UserInfo -> Http.Header
createAuthHeader userInfo =
    Http.header "Auth-Token" ("dating-auth-cookie=" ++ userInfo.authToken)
