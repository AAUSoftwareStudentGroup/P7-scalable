port module DatingApi exposing (User, UserInfo, Credentials, Gender(..), emptyUser, genderToString, stringToGender, ChatMessage, Message, PostMessage, getUserById, getUsers, getMessagesFromId, postUsers, postLogin, getRecentMessages, postMessage)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Http
import String
import Url
import Time as Time

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
    , username : String
    }

type alias Credentials =
    { username : String
    , password : String
    }

emptyUser : User
emptyUser =
    User "" "" "" Other "" "" 0 "" ""

genderToString : Gender -> String
genderToString gender =
    case gender of
        Male ->
            "Male"
        Female ->
            "Female"
        Other ->
            "Other"

stringToGender : String -> Gender
stringToGender str =
    case str of
        "Male" ->
            Male
        "Female" ->
            Female
        _ ->
            Other

type alias ChatMessage =
    { body : String
    , authorId : Int
    , conversationId : Int
    , authorName : String
    , timeStamp : String
    }

type alias Message =
    { body : String
    , convoWithUsername : String
    , convoWithId : Int
    , imLastAuthor : Bool
    , timeStamp : String
    }

type alias PostMessage =
    { convId : Int
    , authorId : Int
    , authorName : String
    , time : String
    , message : String
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

encodeMessage : PostMessage -> Encode.Value
encodeMessage x =
    Encode.object
        [ ( "conversationId", Encode.int x.convId )
        , ( "authorId", Encode.int x.authorId )
        , ( "authorName", Encode.string x.authorName )
        , ( "timeStamp", Encode.string x.time )
        , ( "body", Encode.string x.message )
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

chatMessageDecoder : Decoder ChatMessage
chatMessageDecoder =
    Decode.succeed ChatMessage
        |> Pipeline.required "body" Decode.string
        |> Pipeline.required "authorId" Decode.int
        |> Pipeline.required "conversationId" Decode.int
        |> Pipeline.required "authorName" Decode.string
        |> Pipeline.required "timeStamp" Decode.string


messageDecoder : Decoder Message
messageDecoder =
    Decode.succeed Message
        |> Pipeline.required "body" Decode.string
        |> Pipeline.required "convoWithUsername" Decode.string
        |> Pipeline.required "convoWithId" Decode.int
        |> Pipeline.required "imLastAuthor" Decode.bool
        |> Pipeline.required "timeStamp" Decode.string


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

postMessage : UserInfo -> PostMessage -> Int -> Http.Request (String.String)
postMessage userInfo message userId =
    Http.request
        { method =
            "POST"
        , headers =
            [createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "messages"
                , userId |> String.fromInt |> Url.percentEncode
                ]
        , body =
            Http.jsonBody (encodeMessage <| message)
        , expect =
            Http.expectString
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getMessagesFromId : UserInfo -> Int -> Http.Request (List (ChatMessage))
getMessagesFromId userInfo id =
    Http.request
        { method =
            "GET"
        , headers =
            [createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "messages"
                , String.fromInt id
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (Decode.list chatMessageDecoder)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getRecentMessages : UserInfo -> Http.Request (List (Message))
getRecentMessages userInfo =
    Http.request
        { method =
            "GET"
        , headers =
            [createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "messages"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (Decode.list messageDecoder)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

createAuthHeader : UserInfo -> Http.Header
createAuthHeader userInfo =
  Http.header "Auth-Token" ("dating-auth-cookie=" ++ userInfo.authToken)
