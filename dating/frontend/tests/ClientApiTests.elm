module ClientApiTests exposing (suite)

import ClientApi exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeValue)
import Json.Encode
import Test exposing (..)


suite : Test
suite =
    describe "The ClientApi module"
        [ describe "decodeUser"
            [ test "decodes valid user" <|
                \_ ->
                    let
                        json =
                            jsonUserValid
                    in
                    decodeValue decodeUser json
                        |> Expect.equal
                            (Ok typedUserValid)
            ]
        ]


-- Quick solution. We should be using fuzz testing!

jsonUserValid : Json.Encode.Value
jsonUserValid =
    Json.Encode.object
        [ ( "userEmail", Json.Encode.string "kasper@bargsteen.com" )
        , ( "userPassword", Json.Encode.string "Hunter2" )
        , ( "userUsername", Json.Encode.string "Bargsteen" )
        , ( "userGender", encodeGender Male )
        , ( "userBirthday", Json.Encode.string "1994-05-06" )
        , ( "userTown", Json.Encode.string "Aalborg" )
        , ( "userProfileText", Json.Encode.string "I like big butts and I cannot lie" )
        ]


typedUserValid : User
typedUserValid =
    { userEmail = "kasper@bargsteen.com"
    , userPassword = "Hunter2"
    , userUsername = "Bargsteen"
    , userGender = Male
    , userBirthday = "1994-05-06"
    , userTown = "Aalborg"
    , userProfileText = "I like big butts and I cannot lie"
    }
