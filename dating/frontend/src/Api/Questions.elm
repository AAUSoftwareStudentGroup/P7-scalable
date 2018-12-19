module Api.Questions exposing (Answer, Question, emptyAnswer, emptyQuestion, getQuestions, postAnswer)

import Api.ApiLocation exposing (apiLocation)
import Api.Authentication as Auth exposing (UserInfo)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode


type alias Question =
    { id : String
    , question : String
    }


type alias Answer =
    { id : String
    , score : Int
    }


emptyQuestion : Question
emptyQuestion =
    Question "" ""


emptyAnswer : Answer
emptyAnswer =
    Answer "" -1


decodeQuestions : Decoder Question
decodeQuestions =
    Decode.succeed Question
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "question" Decode.string


encodeAnswer : Answer -> Encode.Value
encodeAnswer answer =
    Encode.object
        [ ( "id", Encode.string answer.id )
        , ( "score", Encode.int answer.score )
        ]


getQuestions : UserInfo -> Http.Request (List Question)
getQuestions userInfo =
    Http.request
        { method =
            "GET"
        , headers =
            [ Auth.createAuthHeader userInfo ]
        , url =
            String.join "/"
                [ apiLocation
                , "questions"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (Decode.list decodeQuestions)
        , timeout =
            Nothing
        , withCredentials =
            False
        }


postAnswer : UserInfo -> Answer -> Http.Request String.String
postAnswer userInfo answer =
    Http.request
        { method =
            "POST"
        , headers =
            [ Auth.createAuthHeader userInfo ]
        , url =
            String.join "/"
                [ apiLocation
                , "questions"
                ]
        , body =
            Http.jsonBody (encodeAnswer answer)
        , expect =
            Http.expectString
        , timeout =
            Nothing
        , withCredentials =
            False
        }
