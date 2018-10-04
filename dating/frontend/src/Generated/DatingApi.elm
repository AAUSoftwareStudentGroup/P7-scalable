module Generated.DatingApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Url

import GenHelpers exposing (..)

type alias User =
    { userEmail : String
    , userPassword : String
    , userUsername : String
    , userGender : Gender
    , userBirthday : String
    , userTown : String
    , userProfileText : String
    , userAuthToken : String
    , id : Int
    }

type alias Credentials =
    { username : String
    , password : String
    }

decodeUser : Decoder User
decodeUser =
    succeed User
        |> required "email" string
        |> required "password" string
        |> required "username" string
        |> required "gender" decodeGender
        |> required "birthday" string
        |> required "town" string
        |> required "profileText" string
        |> required "authToken" string
        |> required "id" int

decodeCredentials : Decoder Credentials
decodeCredentials =
    succeed Credentials
        |> required "username" string
        |> required "password" string

encodeUser : User -> Json.Encode.Value
encodeUser x =
    Json.Encode.object
        [ ( "email", Json.Encode.string x.userEmail )
        , ( "password", Json.Encode.string x.userPassword )
        , ( "username", Json.Encode.string x.userUsername )
        , ( "gender", encodeGender x.userGender )
        , ( "birthday", Json.Encode.string x.userBirthday )
        , ( "town", Json.Encode.string x.userTown )
        , ( "profileText", Json.Encode.string x.userProfileText )
        , ( "authToken", Json.Encode.string x.userAuthToken )
        , ( "id", Json.Encode.int x.id )
        ]

encodeCredentials : Credentials -> Json.Encode.Value
encodeCredentials x =
    Json.Encode.object
        [ ( "username", Json.Encode.string x.username )
        , ( "password", Json.Encode.string x.password )
        ]

getUsersByUserid : Int -> Http.Request (User)
getUsersByUserid capture_userid =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "users"
                , capture_userid |> String.fromInt |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeUser
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getUsers : String -> Http.Request (List (User))
getUsers token =
    Http.request
        { method =
            "GET"
        , headers =
            [Http.header "Auth-Token" ("dating-auth-cookie=" ++ token)]
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "users"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeUser)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postUsers : User -> Http.Request (Int)
postUsers body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "users"
                ]
        , body =
            Http.jsonBody (encodeUser body)
        , expect =
            Http.expectJson int
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postLogin : Credentials -> Http.Request (String)
postLogin body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ "http://localhost:8080"
                , "login"
                ]
        , body =
            Http.jsonBody (encodeCredentials body)
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }