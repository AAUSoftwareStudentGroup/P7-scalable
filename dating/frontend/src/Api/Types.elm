module Api.Types exposing (Gender(..), Image, genderToString)


type Gender
    = Male
    | Female
    | Other

type alias Image =
    { contents : String
    , filename : String
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
