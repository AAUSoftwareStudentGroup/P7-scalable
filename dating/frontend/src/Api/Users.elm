module Api.Users exposing (NewUser, EditUserDTO, User, getAge, postUsers, postLogin, postEditUser, postLogout, getUserByUsername, getUserAlreadyExists, getUsers, getMatches, emptyUser, encodeUserInfo, decodeUserInfo)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Http
import String
import Url
import Time
import Date exposing(Date, diff, Unit(..))

import Api.Types exposing (Gender(..))
import Api.Authentication as Auth exposing (UserInfo, Token, Credentials)

import Api.ApiLocation exposing (apiLocation)

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

type alias EditUserDTO =
    { password      : Maybe String
    , gender        : Maybe Gender
    , birthday      : Maybe String
    , town          : Maybe String
    , profileText   : Maybe String
    , image         : Maybe String
    }

type alias User =
    { username      : String
    , gender        : Gender
    , birthday      : Date
    , town          : String
    , profileText   : String
    , image         : String
    }

emptyUser : User
emptyUser =
    User "" Other (Date.fromOrdinalDate 0 1) "" "" ""


getAge : User -> Time.Posix -> Int
getAge user now =
    Date.diff Years user.birthday <| Date.fromPosix Time.utc now

encodeDate : Date -> Encode.Value
encodeDate date =
    Encode.string (Api.Types.dateToString date)

decodeDate : String -> Decoder Date
decodeDate fieldName =
    Decode.field fieldName Decode.string
        |> Decode.andThen decodeDateHelper

decodeDateHelper : String -> Decoder Date
decodeDateHelper str =
    Decode.succeed <| (Api.Types.stringToDate str)

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


encodeNewUser : NewUser -> Encode.Value
encodeNewUser user =
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

encodeEditUser : EditUserDTO -> Encode.Value
encodeEditUser user =
    Encode.object
        [ ( "password", encodeMaybe Encode.string user.password )
        , ( "gender", encodeMaybe encodeGender user.gender )
        , ( "birthday", encodeMaybe Encode.string user.birthday )
        , ( "town", encodeMaybe Encode.string user.town )
        , ( "profileText", encodeMaybe Encode.string user.profileText )
        , ( "imageData", encodeMaybe Encode.string user.image )
        ]

encodeMaybe : (dataType -> Encode.Value) -> Maybe dataType -> Encode.Value
encodeMaybe encoder maybeData =
    case maybeData of
        Just data ->
            encoder data
        Nothing ->
            Encode.null

encodeUserInfo : UserInfo -> Encode.Value
encodeUserInfo userInfo =
    Encode.object
        [ ( "authToken", Encode.string userInfo.authToken )
        , ( "username",  Encode.string userInfo.username )
        , ( "firstLogIn", Encode.bool userInfo.firstLogIn )
        ]

decodeUser : Decoder User
decodeUser =
    Decode.succeed User
        |> Pipeline.required "username" Decode.string
        |> Pipeline.custom decodeGender
        |> Pipeline.custom (decodeDate "birthday")
        |> Pipeline.required "town" Decode.string
        |> Pipeline.required "profileText" Decode.string
        |> Pipeline.required "imageUrl" Decode.string


decodeUserInfo : Decoder UserInfo
decodeUserInfo =
    Decode.succeed UserInfo
        |> Pipeline.required "authToken" Decode.string
        |> Pipeline.required "username" Decode.string
        |> Pipeline.required "firstLogIn" Decode.bool


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
            Http.jsonBody (encodeNewUser body)
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


postLogout : UserInfo -> Http.Request (String.String)
postLogout userInfo =
    Http.request
        { method =
            "POST"
        , headers =
            [Auth.createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "logout"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectString
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getUserByUsername : UserInfo -> String -> Http.Request (User)
getUserByUsername userInfo username =
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

getUsers : UserInfo -> Int -> Int -> Http.Request (List (User))
getUsers userInfo pageNum pageSize =
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

postEditUser : UserInfo -> EditUserDTO -> Http.Request UserInfo
postEditUser userInfo body =
    Http.request
        { method =
            "POST"
        , headers =
            [Auth.createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "edit"
                ]
        , body =
            Http.jsonBody (encodeEditUser body)
        , expect =
            Http.expectJson decodeUserInfo
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getMatches : UserInfo -> Http.Request (List (User))
getMatches userInfo =
    Http.request
        { method =
            "GET"
        , headers =
            [Auth.createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "match"
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
