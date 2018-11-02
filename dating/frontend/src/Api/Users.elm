module Api.Users exposing (NewUser, User, postUsers, postLogin, postLogout, getUserById, getUsers, emptyUser, encodeUserInfo, decodeUserInfo)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Http
import String
import Url

import Api.Types exposing (Gender(..), UserInfo, Token, Id)
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
    , userId        : Id
    , gender        : Gender
    , birthday      : String
    , town          : String
    , profileText   : String
    }


emptyUser : User
emptyUser =
    User "" "" Other "" "" ""


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
    Decode.field "userDTOGender" Decode.string
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
        [ ( "CreateUserDTOEmail", Encode.string user.email )
        , ( "CreateUserDTOPassword", Encode.string user.password )
        , ( "CreateUserDTOUsername", Encode.string user.username )
        , ( "CreateUserDTOGender", encodeGender user.gender )
        , ( "CreateUserDTOBirthday", Encode.string user.birthday )
        , ( "CreateUserDTOTown", Encode.string user.town )
        , ( "CreateUserDTOProfileText", Encode.string user.profileText )
        ]

encodeUserInfo : UserInfo -> Encode.Value
encodeUserInfo userInfo =
    Encode.object
        [ ( "loggedInDTOUsername",  Encode.string userInfo.username )
        , ( "loggedInDTOUserId", Encode.string userInfo.userId )
        , ( "loggedInDTOAuthToken", encodeToken userInfo.authToken )
        ]

decodeUser : Decoder User
decodeUser =
    Decode.succeed User
        |> Pipeline.required "UserDTOUsername" Decode.string
        |> Pipeline.required "UserDTOUserId" Decode.string
        |> Pipeline.custom decodeGender
        |> Pipeline.required "UserDTOBirthday" Decode.string
        |> Pipeline.required "UserDTOTown" Decode.string
        |> Pipeline.required "UserDTOProfileText" Decode.string


decodeUserInfo : Decoder UserInfo
decodeUserInfo =
    Decode.succeed UserInfo
        |> Pipeline.required "loggedInDTOUsername" Decode.string
        |> Pipeline.required "loggedInDTOUserId" Decode.string
        |> Pipeline.custom decodeToken


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

getUserById : Id -> UserInfo -> Http.Request (User)
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
                , userId |> Url.percentEncode
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