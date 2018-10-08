module GenHelpers exposing (Gender(..), decodeGender, decodeGenderAux, encodeGender)

import Http
import Json.Decode as D
import Json.Encode as E

type Gender
    = Male
    | Female
    | Other


decodeGender : D.Decoder Gender
decodeGender =
    D.field "gender" D.string
        |> D.andThen decodeGenderAux


decodeGenderAux : String -> D.Decoder Gender
decodeGenderAux str =
    case str of
        "Male" ->
            D.succeed Male

        "Female" ->
            D.succeed Female

        "Other" ->
            D.succeed Other

        somethingElse ->
            D.fail <| "Unknown gender: " ++ somethingElse


encodeGender : Gender -> E.Value
encodeGender g =
    case g of
        Male ->
            E.string "Male"

        Female ->
            E.string "Female"

        Other ->
            E.string "Other"
