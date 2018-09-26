module ClientApi exposing (Gender(..), User, getUserByUserid, decodeUser, encodeGender, encodeUser, postUserRequest)

import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode exposing (encode)
import String
import Url.Builder exposing (crossOrigin)


type Gender
    = Male
    | Female
    | Other


type alias User =
    { userEmail : String
    , userPassword : String
    , userUsername : String
    , userGender : Gender
    , userBirthday : String
    , userTown : String
    , userProfileText : String
    }


decodeUser : Decoder User
decodeUser =
    Json.Decode.succeed User
        |> required "userEmail" string
        |> required "userPassword" string
        |> required "userUsername" string
        |> custom gender
        |> required "userBirthday" string
        |> required "userTown" string
        |> required "userProfileText" string


gender : Decoder Gender
gender =
    field "userGender" string
        |> Json.Decode.andThen decodeGender


decodeGender : String -> Decoder Gender
decodeGender str =
    case str of
        "Male" ->
            Json.Decode.succeed Male

        "Female" ->
            Json.Decode.succeed Female

        "Other" ->
            Json.Decode.succeed Other

        somethingElse ->
            Json.Decode.fail <| "Unknown gender: " ++ somethingElse

encodeGender : Gender -> Json.Encode.Value
encodeGender g =
    case g of
        Male -> Json.Encode.string "Male"
        Female -> Json.Encode.string "Female"
        Other -> Json.Encode.string "Other"


encodeUser : User -> Json.Encode.Value
encodeUser user =
    Json.Encode.object
        [ ("userEmail", Json.Encode.string (user.userEmail))
        , ("userPassword", Json.Encode.string (user.userPassword))
        , ("userUsername", Json.Encode.string (user.userUsername))
        , ("userGender", encodeGender (user.userGender))
        , ("userBirthday", Json.Encode.string (user.userBirthday))
        , ("userTown", Json.Encode.string (user.userTown))
        , ("userProfileText", Json.Encode.string (user.userProfileText))
        ]


getUserByUserid : Int -> Http.Request User
getUserByUserid capture_userid =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            theUrl capture_userid
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeUser
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
            baseUrl
        , body =
            Http.jsonBody <| encodeUser user
        , expect =
            Http.expectJson Json.Decode.int
        , timeout =
            Nothing
        , withCredentials =
            False
        }


theUrl : Int -> String
theUrl uid =
    crossOrigin "http://localhost:8080" [ "users", String.fromInt uid ] []

baseUrl : String
baseUrl = crossOrigin "http://localhost:8080" ["users"] []



-- postUsers : User -> Http.Request (Int)
-- postUsers body =
--     Http.request
--         { method =
--             "POST"
--         , headers =
--             []
--         , url =
--             String.join "/"
--                 [ ""
--                 , "users"
--                 ]
--         , body =
--             Http.jsonBody (encodeUser body)
--         , expect =
--             Http.expectJson int
--         , timeout =
--             Nothing
--         , withCredentials =
--             False
--         }
