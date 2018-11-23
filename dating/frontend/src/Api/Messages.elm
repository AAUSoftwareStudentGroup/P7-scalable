module Api.Messages exposing (Message, Conversation, ConversationPreviewDTO, postMessage, getMessagesFromUsername, getConvoPreview)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Http
import String
import Url
import Time as Time
import Iso8601

import Api.Authentication as Auth exposing (UserInfo)


apiLocation : String
apiLocation =
    "http://api.dating.local"


type alias Message =
    { body          : String
    , authorName    : String
    , timeStamp     : Time.Posix
    }

type alias ConversationPreviewDTO =
    { convoWithUsername : String
    , body              : String
    , isLastAuthor      : Bool
    , timeStamp         : Time.Posix
    }

type alias Conversation =
    { convoWithUsername : String
    , messages          : List Message
    }

encodeMessage : String -> Encode.Value
encodeMessage x =
    Encode.object
        [ ( "body", Encode.string x ) ]


decodeMessage : Decoder Message
decodeMessage =
    Decode.succeed Message
        |> Pipeline.required "body" Decode.string
        |> Pipeline.required "authorUsername" Decode.string
        |> Pipeline.required "timeStamp" Iso8601.decoder


decodeConversation : Decoder Conversation
decodeConversation =
    Decode.succeed Conversation
        |> Pipeline.required "convoWithUsername" Decode.string
        |> Pipeline.required "messages" (Decode.list (decodeMessage))


decodeConvoPreview : Decoder ConversationPreviewDTO
decodeConvoPreview =
    Decode.succeed ConversationPreviewDTO
        |> Pipeline.required "convoWithUsername" Decode.string
        |> Pipeline.required "body" Decode.string
        |> Pipeline.required "isLastAuthor" Decode.bool
        |> Pipeline.required "timeStamp" Iso8601.decoder



postMessage : UserInfo -> String -> String -> Http.Request (String.String)
postMessage userInfo message to =
    Http.request
        { method =
            "POST"
        , headers =
            [Auth.createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "messages"
                , to |> Url.percentEncode
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

getMessagesFromUsername : UserInfo -> String -> Int -> Int -> Http.Request Conversation
getMessagesFromUsername userInfo username offset pageSize =
    Http.request
        { method =
            "GET"
        , headers =
            [Auth.createAuthHeader userInfo]
        , url =
            String.join "/"
                [ apiLocation
                , "messages"
                , username
                , String.fromInt offset
                , String.fromInt pageSize
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeConversation
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
