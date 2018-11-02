module Api.Messages exposing (Message, ConversationPreviewDTO, postMessage, getMessagesFromId, getConvoPreview)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Http
import String
import Url
import Time as Time

import Api.Types exposing (UserInfo, Id)
import Api.Authentication as Auth



apiLocation : String
apiLocation =
    "http://api.dating.local:8002"



type alias Message =
    { messageId     : Id
    , authorName    : String
    , timeStamp     : String
    , body          : String
    }

type alias ConversationPreviewDTO =
    { convoWithUsername : String
    , convoWithId       : Id
    , isLastAuthor      : Bool
    , body              : String
    , timeStamp         : String
    }


encodeMessage : String -> Encode.Value
encodeMessage x =
    Encode.object
        [ ( "MessageDTOBody", Encode.string x ) ]



decodeMessage : Decoder Message
decodeMessage =
    Decode.succeed Message
        |> Pipeline.required "MessageDTOMessageId" Decode.string
        |> Pipeline.required "MessageDTOAuthorName" Decode.string
        |> Pipeline.required "MessageDTOTimeStamp" Decode.string
        |> Pipeline.required "MessageDTOBody" Decode.string


decodeConvoPreview : Decoder ConversationPreviewDTO
decodeConvoPreview =
    Decode.succeed ConversationPreview
        |> Pipeline.required "ConvoPreviewDTOConvoWithUsername" Decode.string
        |> Pipeline.required "ConvoPreviewDTOConvoWithId" Decode.string
        |> Pipeline.required "ConvoPreviewDTOImLastAuthor" Decode.bool
        |> Pipeline.required "ConvoPreviewDTOBody" Decode.string
        |> Pipeline.required "ConvoPreviewDTOTimeStamp" Decode.string



postMessage : UserInfo -> String -> Id -> Http.Request (String.String)
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
                , userId |> Url.percentEncode
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

getMessagesFromId : UserInfo -> Id -> Http.Request (List (Message))
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
                , id
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
