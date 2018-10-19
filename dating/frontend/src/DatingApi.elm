port module DatingApi exposing (User, UserInfo, Credentials, Gender(..), getUserById, getUsers, postUsers, postLogin)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Http
import String
import Url


type alias User =
    { userEmail : String
    , userPassword : String
    , userUsername : String
    , userGender : Gender
    , userBirthday : String
    , userTown : String
    , userId : Int
    , userProfileText : String
    , userAuthToken : String
    }

type Gender
    = Male
    | Female
    | Other

type alias UserInfo =
    { userId : Int
    , authToken : String
    }

type alias Credentials =
    { username : String
    , password : String
    }

apiLocation : String
apiLocation =
    "http://api.dating.local:8002"

encodeUser : User -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "email", Encode.string user.userEmail )
        , ( "password", Encode.string user.userPassword )
        , ( "username", Encode.string user.userUsername )
        , ( "gender", encodeGender user.userGender )
        , ( "birthday", Encode.string user.userBirthday )
        , ( "town", Encode.string user.userTown )
        , ( "profileText", Encode.string user.userProfileText )
        , ( "authToken", Encode.string user.userAuthToken )
        ]

encodeGender : Gender -> Encode.Value
encodeGender g =
    case g of
        Male ->
            Encode.string "Male"

        Female ->
            Encode.string "Female"

        Other ->
            Encode.string "Other"


encodeCredentials : Credentials -> Encode.Value
encodeCredentials x =
    Encode.object
        [ ( "username", Encode.string x.username )
        , ( "password", Encode.string x.password )
        ]

userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> Pipeline.required "email" Decode.string
        |> Pipeline.required "password" Decode.string
        |> Pipeline.required "username" Decode.string
        |> Pipeline.custom genderDecoder
        |> Pipeline.required "birthday" Decode.string
        |> Pipeline.required "town" Decode.string
        |> Pipeline.required "id" Decode.int
        |> Pipeline.required "profileText" Decode.string
        |> Pipeline.required "authToken" Decode.string

credentialsDecoder : Decoder Credentials
credentialsDecoder =
    Decode.succeed Credentials
        |> Pipeline.required "username" Decode.string
        |> Pipeline.required "password" Decode.string


genderDecoder : Decoder Gender
genderDecoder =
    Decode.field "gender" Decode.string
        |> Decode.andThen genderDecoderHelper


genderDecoderHelper : String -> Decoder Gender
genderDecoderHelper str =
    case str of
        "Male" ->
            Decode.succeed Male

        "Female" ->
            Decode.succeed Female

        "Other" ->
            Decode.succeed Other

        somethingElse ->
            Decode.fail <| "Unknown gender: " ++ somethingElse


getUserById : Int -> UserInfo -> Http.Request (User)
getUserById userId userInfo =
    Http.request
        { method =
            "GET"
        , headers =
            [createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "users"
                , userId |> String.fromInt |> Url.percentEncode
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson userDecoder
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getUsers : UserInfo -> Http.Request (List (User))
getUsers userInfo =
    Http.request
        { method =
            "GET"
        , headers =
            [createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "users"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (Decode.list userDecoder)
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
                [ apiLocation
                , "users"
                ]
        , body =
            Http.jsonBody (encodeUser body)
        , expect =
            Http.expectJson Decode.int
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postLogin : Credentials -> Http.Request (User)
postLogin body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ apiLocation
                , "login"
                ]
        , body =
            Http.jsonBody (encodeCredentials body)
        , expect =
            Http.expectJson userDecoder
        , timeout =
            Nothing
        , withCredentials =
            False
        }

createAuthHeader : UserInfo -> Http.Header
createAuthHeader userInfo =
  Http.header "Auth-Token" ("dating-auth-cookie=" ++ userInfo.authToken)
