module Api.Types exposing (Gender(..), Image, genderToString, dateToString, stringToDate)
import Date exposing(Date)

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

dateToString : Date -> String
dateToString date =
    Date.toIsoString date

stringToDate : String -> Date
stringToDate dateString =
    case Date.fromIsoString dateString of
        Ok date -> date
        Err error -> Date.fromOrdinalDate 0 1
