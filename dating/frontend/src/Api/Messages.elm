module Api.Messages exposing (Message, ConversationPreviewDTO, postMessage, getMessagesFromId, getConvoPreview)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Http
import String
import Url
import Time as Time

import Api.Types exposing (UserInfo)
import Api.Authentication as Auth



apiLocation : String
apiLocation =
    "http://api.dating.local:8002"



type alias Message =
    { authorName    : String
    , timeStamp     : String
    , body          : String
    }

type alias ConversationPreviewDTO =
    { convoWithUsername : String
    , convoWithId       : Int
    , isLastAuthor      : Bool
    , body              : String
    , timeStamp         : String
    }


encodeMessage : String -> Encode.Value
encodeMessage x =
    Encode.object
        [ ( "body", Encode.string x ) ]



decodeMessage : Decoder Message
decodeMessage =
    Decode.succeed Message
        |> Pipeline.required "authorName" Decode.string
        |> Pipeline.required "timeStamp" Decode.string
        |> Pipeline.required "body" Decode.string


decodeConvoPreview : Decoder ConversationPreviewDTO
decodeConvoPreview =
    Decode.succeed ConversationPreviewDTO
        |> Pipeline.required "convoWithUsername" Decode.string
        |> Pipeline.required "convoWithId" Decode.int
        |> Pipeline.required "imLastAuthor" Decode.bool
        |> Pipeline.required "body" Decode.string
        |> Pipeline.required "timeStamp" Decode.string



postMessage : UserInfo -> String -> Int -> Http.Request (String.String)
postMessage userInfo message userId =
    Http.request
        { method =
            "POST"
        , headers =
            [Auth.createAuthHeader userInfo]
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

getMessagesFromId : UserInfo -> Int -> Http.Request (List (Message))
getMessagesFromId userInfo id =
    Http.request
        { method =
            "GET"
        , headers =
            [Auth.createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "messages"
                , String.fromInt id
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (Decode.list decodeMessage)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getConvoPreview : UserInfo -> Http.Request (List (ConversationPreviewDTO))
getConvoPreview userInfo =
    Http.request
        { method =
            "GET"
        , headers =
            [Auth.createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "messages"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (Decode.list decodeConvoPreview)
        , timeout =
            Nothing
        , withCredentials =
            False
        }
