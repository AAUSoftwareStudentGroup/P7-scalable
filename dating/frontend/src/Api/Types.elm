module Api.Types exposing (Gender(..), UserInfo, Token, Id, genderToString)


type Gender
    = Male
    | Female
    | Other


type alias Token = String

type alias Id = String

type alias UserInfo =
    { username      : String
    , userId        : Id
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