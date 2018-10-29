module Api.Users exposing (NewUser, User, postUsers, postLogin, getUserById, getUsers, emptyUser, encodeUserInfo, decodeUserInfo)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Http
import String
import Url

import Api.Types exposing (Gender(..), UserInfo)
import Api.Authentication as Auth exposing (Credentials)


apiLocation : String
apiLocation =
    "http://api.dating.local:8002"


type alias NewUser =
    { email         : String
    , password      : String
    , username      : String
    , gender        : Gender
    , birthday      : String
    , town          : String
    , profileText   : String
    }

type alias User =
    { username      : String
    , userId        : Int
    , gender        : Gender
    , birthday      : String
    , town          : String
    , profileText   : String
    }


emptyUser : User
emptyUser =
    User "" -1 Other "" "" ""


encodeGender : Gender -> Encode.Value
encodeGender g =
    case g of
        Male ->
            Encode.string "Male"

        Female ->
            Encode.string "Female"

        Other ->
            Encode.string "Other"



decodeGender : Decoder Gender
decodeGender =
    Decode.field "gender" Decode.string
        |> Decode.andThen decodeGenderHelper


decodeGenderHelper : String -> Decoder Gender
decodeGenderHelper str =
    case str of
        "Male" ->
            Decode.succeed Male

        "Female" ->
            Decode.succeed Female

        "Other" ->
            Decode.succeed Other

        somethingElse ->
            Decode.fail <| "Unknown gender: " ++ somethingElse


encodeUser : NewUser -> Encode.Value
encodeUser user =
    Encode.object
        [ ( "email", Encode.string user.email )
        , ( "password", Encode.string user.password )
        , ( "username", Encode.string user.username )
        , ( "gender", encodeGender user.gender )
        , ( "birthday", Encode.string user.birthday )
        , ( "town", Encode.string user.town )
        , ( "profileText", Encode.string user.profileText )
        ]

encodeUserInfo : UserInfo -> Encode.Value
encodeUserInfo userInfo =
    Encode.object
        [ ( "userId", Encode.int userInfo.userId )
        , ( "authToken", Encode.string userInfo.authToken )
        , ( "userUsername", Encode.string userInfo.username )
        ]

decodeUser : Decoder User
decodeUser =
    Decode.succeed User
        |> Pipeline.required "username" Decode.string
        |> Pipeline.required "id" Decode.int
        |> Pipeline.custom decodeGender
        |> Pipeline.required "birthday" Decode.string
        |> Pipeline.required "town" Decode.string
        |> Pipeline.required "profileText" Decode.string


decodeUserInfo : Decoder UserInfo
decodeUserInfo =
    Decode.succeed UserInfo
        |> Pipeline.required "userId" Decode.int
        |> Pipeline.required "authToken" Decode.string
        |> Pipeline.required "userUsername" Decode.string


postUsers : NewUser -> Http.Request (UserInfo)
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
            Http.expectJson decodeUserInfo
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postLogin : Credentials -> Http.Request (UserInfo)
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
            Http.jsonBody (Auth.credentialsEncoder body)
        , expect =
            Http.expectJson decodeUserInfo
        , timeout =
            Nothing
        , withCredentials =
            False
        }


getUserById : Int -> UserInfo -> Http.Request (User)
getUserById userId userInfo =
    Http.request
        { method =
            "GET"
        , headers =
            [Auth.createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "users"
                , userId |> String.fromInt |> Url.percentEncode
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

getUsers : UserInfo -> Http.Request (List (User))
getUsers userInfo =
    Http.request
        { method =
            "GET"
        , headers =
            [Auth.createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "users"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (Decode.list decodeUser)
        , timeout =
            Nothing
        , withCredentials =
            False
        }