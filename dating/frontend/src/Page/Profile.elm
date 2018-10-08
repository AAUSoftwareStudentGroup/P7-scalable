module Main exposing (Model(..), Msg(..), blue, darkBlue, emptyUser, grey, init, main, mkWarning, noLabel, pure, red, subscriptions, update, view, white)

import Browser
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import GenHelpers exposing (Gender(..))
import Generated.DatingApi exposing (..)
import Http
import Page.Header
import String


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model emptyUser, Http.send HandleGetUser (getUsersByUserid 14) )


emptyUser : User
emptyUser =
    User "" "" "" Other "" "" "" ""


type Msg
    = GetUser Int
    | Update User
    | HandleGetUser (Result Http.Error User)


type Model
    = Model User


subscriptions userEntries =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model user) =
    case msg of
        Update newFormEntries ->
            pure (Model newFormEntries)

        GetUser userid ->
            ( Model user, Http.send HandleGetUser (getUsersByUserid userid) )

        HandleGetUser result ->
            case result of
                Ok fetchedUser ->
                    pure (Model fetchedUser)

                Err errResponse ->
                    pure <| Debug.log (Debug.toString errResponse) <| Model user


pure : Model -> ( Model, Cmd Msg )
pure userEntries =
    ( userEntries, Cmd.none )


view : Model -> Browser.Document Msg
view (Model userEntries) =
    { title = userEntries.userUsername ++ "'s profile"
    , body =
        [ Element.layout
            [ Font.size 20
            ]
          <|
            Element.column [ width fill ]
                [ Element.column [ width fill, height shrink ]
                    [ Page.Header.getHeader Nothing ]
                , Element.column [ width (px 800), height shrink, centerY, centerX, spacing 36, padding 10 ]
                    [ el
                        [ Region.heading 1
                        , centerX
                        , Font.size 36
                        , Border.color darkBlue
                        ]
                        (text (userEntries.userUsername ++ "'s profile"))
                    , el [ spacing 12, Border.color darkBlue ]
                        (text userEntries.userUsername)
                    , el [ spacing 12, Border.color darkBlue ]
                        (text userEntries.userEmail)
                    , el [ spacing 12, Border.color darkBlue ]
                        (text <| genderToString userEntries.userGender)
                    , el [ spacing 12, Border.color darkBlue ]
                        (text userEntries.userBirthday)
                    , el [ spacing 12, Border.color darkBlue ]
                        (text userEntries.userTown)
                    , paragraph [ spacing 12, Border.color darkBlue ]
                        [ text userEntries.userProfileText ]
                    ]
                ]
        ]
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


mkWarning warning =
    el
        [ Font.color red
        , Font.size 14
        , alignRight
        , moveDown 6
        ]
        (text warning)


noLabel =
    Input.labelAbove [] none


white =
    Element.rgb 1 1 1


grey =
    Element.rgb 0.9 0.9 0.9


blue =
    Element.rgb 0 0 0.8


red =
    Element.rgb 0.8 0 0


darkBlue =
    Element.rgb 0 0 0.9
