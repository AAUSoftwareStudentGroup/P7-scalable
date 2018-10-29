module Api.Types exposing (Gender(..), UserInfo, genderToString)


type Gender
    = Male
    | Female
    | Other


type alias UserInfo =
    { userId        : Int
    , authToken     : String
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