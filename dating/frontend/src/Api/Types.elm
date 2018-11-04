module Api.Types exposing (Gender(..), UserInfo, Token, genderToString)


type Gender
    = Male
    | Female
    | Other


type alias Token = String

type alias UserInfo =
    { username      : String
    , authToken     : Token
    }


genderToString : Gender -> String
genderToString gender =
    case gender of
        Male ->
            "Male"
        Female ->
            "Female"
        Other ->
            "Other"