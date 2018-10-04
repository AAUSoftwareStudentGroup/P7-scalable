module UserApiTests exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeValue)
import Json.Encode
import Test exposing (..)


suite : Test
suite =
    describe "UserApi module"
        [ describe "genderDecoder (male)"
            [ test "decodes valid json (male)" <|
                \_ ->
                    decodeValue genderDecoder validJsonGenderMale
                        |> Expect.equal
                            (Ok validGenderMale)
            ]
        , describe "genderDecoder (female)"
            [ test "decodes valid json (female)" <|
                \_ ->
                    decodeValue genderDecoder validJsonGenderFemale
                        |> Expect.equal
                            (Ok validGenderFemale)
            ]
        , describe "genderDecoder (other)"
            [ test "decodes valid json (other)" <|
                \_ ->
                    decodeValue genderDecoder validJsonGenderOther
                        |> Expect.equal
                            (Ok validGenderOther)
            ]
        , describe "userDecoder"
            [ test "decodes valid user json" <|
                \_ ->
                    decodeValue userDecoder validJsonUser
                        |> Expect.equal
                            (Ok validUser)
            ]
        , describe "userDecoders"
            [ test "decodes valid json list of users" <|
                \_ ->
                    decodeValue userDecoder validJsonUser
                        |> Expect.equal
                            (Ok validUser)
            ]
        , describe "encodeUser"
            [ test "encodes a valid user" <|
                \_ ->
                    encodeUser validUser
                        |> Expect.equal
                            validJsonUser
            ]
        ]



-- Quick solution. We should be using fuzz testing!


validJsonUser : Json.Encode.Value
validJsonUser =
    Json.Encode.object
        [ ( "userEmail", Json.Encode.string "kasper@bargsteen.com" )
        , ( "userPassword", Json.Encode.string "Hunter2" )
        , ( "userUsername", Json.Encode.string "Bargsteen" )
        , ( "userBirthday", Json.Encode.string "1994-05-06" )
        , ( "userTown", Json.Encode.string "Aalborg" )
        , ( "userProfileText", Json.Encode.string "I like big butts and I cannot lie" )
        , ( "userGender", encodeGender Male )
        ]

validJsonGenderMale : Json.Encode.Value
validJsonGenderMale =
  Json.Encode.object
    [ ("userGender", Json.Encode.string "Male" ) ]

validJsonGenderFemale : Json.Encode.Value
validJsonGenderFemale =
  Json.Encode.object
    [ ("userGender", Json.Encode.string "Female" ) ]

validJsonGenderOther : Json.Encode.Value
validJsonGenderOther =
  Json.Encode.object
    [ ("userGender", Json.Encode.string "Other" ) ]

validGenderMale : Gender
validGenderMale = Male

validGenderFemale : Gender
validGenderFemale = Female

validGenderOther : Gender
validGenderOther = Other

validUser : User
validUser =
    { userEmail = "kasper@bargsteen.com"
    , userPassword = "Hunter2"
    , userUsername = "Bargsteen"
    , userBirthday = "1994-05-06"
    , userTown = "Aalborg"
    , userProfileText = "I like big butts and I cannot lie"
    , userGender = Male
    }
