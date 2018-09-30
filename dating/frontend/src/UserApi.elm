module UserApi exposing (..)

import Http
import String

import Json.Decode exposing (..)
import Json.Decode.Pipeline as Pipeline exposing (..)
import Json.Encode exposing (encode)
import Url.Builder as URLBuilder exposing (crossOrigin)
import ClientApi exposing (apiUrl)


type Gender
    = Male
    | Female
    | Other


type alias User =
    { userEmail : String
    , userUsername : String
    , userPassword : String
    , userBirthday : String
    , userTown : String
    , userProfileText : String
    , userGender : Gender
    }


getUser : (Result Http.Error User -> msg) -> Int -> Cmd msg
getUser responseMsg userId =
    Http.send responseMsg (getUserRequest userId)

getUsers : (Result Http.Error (List User) -> msg) -> Cmd msg
getUsers responseMsg =
    Http.send responseMsg (getUsersRequest)

createUser : (Result Http.Error Int -> msg) -> User -> Cmd msg
createUser responseMsg user =
    Http.send responseMsg (postUserRequest user)

getUserRequest : Int -> Http.Request User
getUserRequest captureUserId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            getUserUrl captureUserId
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeUser
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getUsersRequest : Http.Request (List User)
getUsersRequest =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            usersUrl
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeUsers
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postUserRequest : User -> Http.Request Int
postUserRequest user =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            postUserUrl
        , body =
            Http.jsonBody <| encodeUser user
        , expect =
            Http.expectJson Json.Decode.int
        , timeout =
            Nothing
        , withCredentials =
            False
        }

usersUrl : String
usersUrl =
    crossOrigin apiUrl [ "users" ] []

getUserUrl : Int -> String
getUserUrl uid =
    crossOrigin apiUrl [ "users", String.fromInt uid ] []

postUserUrl : String
postUserUrl =
    usersUrl

decodeUsers : Decoder (List User)
decodeUsers =
    Json.Decode.list decodeUser

decodeUser : Decoder User
decodeUser =
    Json.Decode.succeed User
        |> Pipeline.required "userEmail" string
        |> Pipeline.required "userUsername" string
        |> Pipeline.required "userPassword" string
        |> Pipeline.required "userBirthday" string
        |> Pipeline.required "userTown" string
        |> Pipeline.required "userProfileText" string
        |> Pipeline.custom decodeGender


decodeGender : Decoder Gender
decodeGender =
    field "userGender" string
        |> Json.Decode.andThen decodeGenderAux


decodeGenderAux : String -> Decoder Gender
decodeGenderAux str =
    case str of
        "Male" ->
            Json.Decode.succeed Male

        "Female" ->
            Json.Decode.succeed Female

        "Other" ->
            Json.Decode.succeed Other

        somethingElse ->
            Json.Decode.fail <| "Unknown gender: " ++ somethingElse

encodeUser : User -> Json.Encode.Value
encodeUser user =
    Json.Encode.object
        [ ("userEmail", Json.Encode.string (user.userEmail))
        , ("userUsername", Json.Encode.string (user.userUsername))
        , ("userPassword", Json.Encode.string (user.userPassword))
        , ("userBirthday", Json.Encode.string (user.userBirthday))
        , ("userTown", Json.Encode.string (user.userTown))
        , ("userProfileText", Json.Encode.string (user.userProfileText))
        , ("userGender", encodeGender (user.userGender))
        ]

encodeGender : Gender -> Json.Encode.Value
encodeGender g =
    case g of
        Male -> Json.Encode.string "Male"
        Female -> Json.Encode.string "Female"
        Other -> Json.Encode.string "Other"
