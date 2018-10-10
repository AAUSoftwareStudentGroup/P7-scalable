module Page.Profile exposing (Model, Msg(..), blue, darkBlue, emptyUser, grey, init, mkWarning, noLabel, red, subscriptions, update, view, white)

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
import String
import Session
import Skeleton


type alias Model =
    { session : Session.Data
    , title : String
    , content : Content
    , user : User
    }

emptyUser : User
emptyUser =
    User "" "" "" Other "" "" "" ""


type Msg
    = GetUser Int
    | HandleGetUser (Result Http.Error User)


type Content
    = Content User


init : Session.Data -> ( Model, Cmd Msg )
init session =
  ( Model session "Profile" (Content emptyUser) emptyUser
  , (Http.send HandleGetUser (getUsersByUserid 8))
  )


subscriptions userEntries =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetUser userid ->
            ( model, Http.send HandleGetUser (getUsersByUserid userid) )

        HandleGetUser result ->
            case result of
                Ok fetchedUser ->
                    ( {model | user = fetchedUser }, Cmd.none)

                Err errResponse ->
                   Debug.log (Debug.toString errResponse) ( { model | user = emptyUser }, Cmd.none )


--view : Model -> Skeleton.Details msg
view model =
    { title = model.user.userUsername ++ "'s profile"
    , kids = [
        Element.column [ width (px 800), height shrink, centerY, centerX, spacing 36, padding 10 ]
            [ el
                [ Region.heading 1
                , centerX
                , Font.size 36
                , Border.color darkBlue
                ]
                (text (model.user.userUsername ++ "'s profile"))
            , el [ spacing 12, Border.color darkBlue ]
                (text model.user.userUsername)
            , el [ spacing 12, Border.color darkBlue ]
                (text model.user.userEmail)
            , el [ spacing 12, Border.color darkBlue ]
                (text <| genderToString model.user.userGender)
            , el [ spacing 12, Border.color darkBlue ]
                (text model.user.userBirthday)
            , el [ spacing 12, Border.color darkBlue ]
                (text model.user.userTown)
            , paragraph [ spacing 12, Border.color darkBlue ]
                [ text model.user.userProfileText ]
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
