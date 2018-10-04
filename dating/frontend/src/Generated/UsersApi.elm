module Generated.UsersApi exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Url

import Models

type alias User =
    { userEmail : String
    , userPassword : String
    , userUsername : String
    , userGender : Gender
    , userBirthday : Date
    , userTown : String
    , userProfileText : String
    , userAuthToken : String
    , id : Int
    }

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "userEmail" string
        |> required "userPassword" string
        |> required "userUsername" string
        |> required "userGender" decodeGender
        |> required "userBirthday" decodeDate
        |> required "userTown" string
        |> required "userProfileText" string
        |> required "userAuthToken" string
        |> required "id" int

encodeUser : User -> Json.Encode.Value
encodeUser x =
    Json.Encode.object
        [ ( "userEmail", Json.Encode.string x.userEmail )
        , ( "userPassword", Json.Encode.string x.userPassword )
        , ( "userUsername", Json.Encode.string x.userUsername )
        , ( "userGender", encodeGender x.userGender )
        , ( "userBirthday", (Json.Encode.string << toString) x.userBirthday )
        , ( "userTown", Json.Encode.string x.userTown )
        , ( "userProfileText", Json.Encode.string x.userProfileText )
        , ( "userAuthToken", Json.Encode.string x.userAuthToken )
        , ( "id", Json.Encode.int x.id )
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

getUsers : Http.Request (List (User))
getUsers =
    Http.request
        { method =
            "GET"
        , headers =
            []
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