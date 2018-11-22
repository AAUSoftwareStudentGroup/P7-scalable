module Api.Users exposing (NewUser, User, postUsers, postLogin, postLogout, getUserByUsername, getUserAlreadyExists, getUsers, emptyUser, encodeUserInfo, decodeUserInfo)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Http
import String
import Url

import Api.Types exposing (Gender(..))
import Api.Authentication as Auth exposing (UserInfo, Token, Credentials)


apiLocation : String
apiLocation =
    "http://api.dating.local"


type alias NewUser =
    { email         : String
    , password      : String
    , username      : String
    , gender        : Gender
    , birthday      : String
    , town          : String
    , profileText   : String
    , image         : String
    }

type alias User =
    { username      : String
    , gender        : Gender
    , birthday      : String
    , town          : String
    , profileText   : String
    , image         : String
    }


emptyUser : User
emptyUser =
    User "" Other "" "" "" ""


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


decodeToken : Decoder Token
decodeToken =
    Decode.field "loggedInDTOAuthToken" Decode.string


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
        , ( "imageData", Encode.string user.image )
        ]

encodeUserInfo : UserInfo -> Encode.Value
encodeUserInfo userInfo =
    Encode.object
        [ ( "authToken", Encode.string userInfo.authToken )
        , ( "username",  Encode.string userInfo.username )
        ]

decodeUser : Decoder User
decodeUser =
    Decode.succeed User
        |> Pipeline.required "username" Decode.string
        |> Pipeline.custom decodeGender
        |> Pipeline.required "birthday" Decode.string
        |> Pipeline.required "town" Decode.string
        |> Pipeline.required "profileText" Decode.string
        |> Pipeline.required "imageUrl" Decode.string


decodeUserInfo : Decoder UserInfo
decodeUserInfo =
    Decode.succeed UserInfo
        |> Pipeline.required "authToken" Decode.string
        |> Pipeline.required "username" Decode.string


encodeToken : Token -> Encode.Value
encodeToken token =
    Encode.string token


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


postLogout : Token -> Http.Request (String.String)
postLogout token =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ apiLocation
                , "logout"
                ]
        , body =
            Http.jsonBody (encodeToken token)
        , expect =
            Http.expectString
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getUserByUsername : String -> UserInfo -> Http.Request (User)
getUserByUsername username userInfo =
    Http.request
        { method =
            "GET"
        , headers =
            [Auth.createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "users"
                , username |> Url.percentEncode
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

getUserAlreadyExists : String -> Http.Request (Bool)
getUserAlreadyExists username =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ apiLocation
                , "users"
                , "exists"
                , username
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson Decode.bool
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getUsers : Int -> Int -> UserInfo -> Http.Request (List (User))
getUsers pageNum pageSize userInfo =
    Http.request
        { method =
            "GET"
        , headers =
            [Auth.createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "users"
                , String.fromInt (pageNum * pageSize)
                , String.fromInt pageSize
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