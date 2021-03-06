module Api.Messages exposing (Conversation, ConversationPreview, Message, emptyConvoPreview, emptyMessage, getConvoPreview, getMessagesFromUsername, postMessage)

import Api.ApiLocation exposing (apiLocation)
import Api.Authentication as Auth exposing (UserInfo)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import String
import Time as Time
import Url


type alias Message =
    { body : String
    , authorName : String
    , timeStamp : Time.Posix
    }


type alias ConversationPreview =
    { convoWithUsername : String
    , body : String
    , isLastAuthor : Bool
    , timeStamp : Time.Posix
    }


type alias Conversation =
    { convoWithUsername : String
    , messages : List Message
    }


emptyConvoPreview : ConversationPreview
emptyConvoPreview =
    ConversationPreview "" "" True <| Time.millisToPosix 0


emptyMessage : Message
emptyMessage =
    Message "" "" <| Time.millisToPosix 0


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
        |> Pipeline.required "messages" (Decode.list decodeMessage)


decodeConvoPreview : Decoder ConversationPreview
decodeConvoPreview =
    Decode.succeed ConversationPreview
        |> Pipeline.required "convoWithUsername" Decode.string
        |> Pipeline.required "body" Decode.string
        |> Pipeline.required "isLastAuthor" Decode.bool
        |> Pipeline.required "timeStamp" Iso8601.decoder


postMessage : UserInfo -> String -> String -> Http.Request String.String
postMessage userInfo message to =
    Http.request
        { method =
            "POST"
        , headers =
            [ Auth.createAuthHeader userInfo ]
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
            [ Auth.createAuthHeader userInfo ]
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


getConvoPreview : UserInfo -> Http.Request (List ConversationPreview)
getConvoPreview userInfo =
    Http.request
        { method =
            "GET"
        , headers =
            [ Auth.createAuthHeader userInfo ]
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
