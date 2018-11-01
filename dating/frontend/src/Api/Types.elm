module Api.Types exposing (Gender(..), UserInfo, Token, genderToString)


type Gender
    = Male
    | Female
    | Other


type alias Token = String

type alias UserInfo =
    { userId        : Int
    , authToken     : Token
    , username      : String
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