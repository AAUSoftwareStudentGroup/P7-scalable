module UserApiTests exposing (suite)

import UserApi exposing (..)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode exposing (decodeValue)
import Json.Encode
import Test exposing (..)


suite : Test
suite =
    describe "UserApi module"
        [ describe "genderDecoder"
            [ test "decodes valid json (male)" <|
                \_ ->
                    decodeValue genderDecoder validJsonGenderMale
                        |> Expect.equal
                            (Ok validGenderMale)
            ]
        , describe "genderDecoder"
            [ test "decodes valid json (female)" <|
                \_ ->
                    decodeValue genderDecoder validJsonGenderFemale
                        |> Expect.equal
                            (Ok validGenderFemale)
            ]
        , describe "genderDecoder"
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


validJsonUser : Json.Decode.Value
validJsonUser = Json.Encode.string """
{
"userBirthday":"1994-05-06",
"userTown":"Aalborg",
"userPassword":"bargsteen",
"userGender":"Male",
"userUsername":"repsak",
"userProfileText":"Some text about me",
"userEmail":"kasper@bargsteen.com"
}
"""

validJsonGenderMale : Json.Decode.Value
validJsonGenderMale = Json.Encode.string "Male"

validJsonGenderFemale : Json.Decode.Value
validJsonGenderFemale = Json.Encode.string "Female"

validJsonGenderOther : Json.Decode.Value
validJsonGenderOther = Json.Encode.string "Other"

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
