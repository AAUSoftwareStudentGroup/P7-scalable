module Page.Profile exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Events as Events
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import String

import DatingApi as Api exposing (Gender(..), User)
import Session exposing (Session)
import Routing exposing (Route(..))
import Skeleton



type alias Model =
    { session : Session
    , title : String
    , id : Int
    , user : User
    }

emptyUser : User
emptyUser =
    User "" "" "" Other "" "" 0 "" ""


type Msg
    = HandleGetUser (Result Http.Error (User))
    | LogoutClicked
    | SessionChanged Session


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
  ( Model session "Profile" id emptyUser
  , (sendGetUser HandleGetUser id session)
  )



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleGetUser result ->
            case result of
                Ok fetchedUser ->
                    ( {model | user = fetchedUser }, Cmd.none)
                Err errResponse ->
                   Debug.log (Debug.toString errResponse) ( { model | user = emptyUser }, Cmd.none )

        LogoutClicked ->
            ( model, Session.logout )

        SessionChanged session ->
            case session of
                Session.Guest key ->
                    ( { model | session = session }
                    , Routing.replaceUrl key (Routing.routeToString Home)
                    )
                Session.LoggedIn key _ ->
                    ( { model | session = session }
                    , Routing.replaceUrl key (Routing.routeToString ListUsers)
                    )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Session.onChange SessionChanged (Session.getNavKey model.session)

view : Model -> Skeleton.Details Msg
view model =
    { title = model.user.userUsername ++ "'s profile"
    , session = model.session
    , kids = [
        Element.column [ width (px 600), height shrink, centerY, centerX, spacing 36, padding 10 ] [
        el  [ Region.heading 1
            , centerX
            , Font.size 36
            , Border.color darkBlue
            ] (text (model.user.userUsername ++ "'s profile"))
            , Element.row [ width (px 600), height shrink, centerY, centerX] [
                Element.column [ width fill, height fill, spacing 36, padding 10]
                    [ el [ spacing 12, Border.color darkBlue ]
                        (text <| "Username: " ++ model.user.userUsername)
                    , el [ spacing 12, Border.color darkBlue ]
                        (text <| "Email: " ++ model.user.userEmail)
                    , el [ spacing 12, Border.color darkBlue ]
                        (text <| "Gender: " ++ genderToString model.user.userGender)
                    , el [ spacing 12, Border.color darkBlue ]
                        (text <| "Birthday: " ++ model.user.userBirthday)
                    , el [ spacing 12, Border.color darkBlue ]
                        (text model.user.userTown)
                    , Element.column [spacing 12, Border.color darkBlue] [
                        el [] (text ("Description"))
                        ,  paragraph [ spacing 12, Border.color darkBlue ]
                           [ text model.user.userProfileText ]
                    ]
                ]
            , Element.column [Element.alignTop] [
                createButtonRight (Routing.routeToString <| (Chat model.user.userId)) "chat"
            ,   createButtonRight (Routing.routeToString <| ListUsers) "listUsers"
            ]
            , el [Events.onClick LogoutClicked] (text "Logout")
            ]
        ]
    ]}





createButtonRight url caption =
    createButton [ alignRight ] url caption


createButton attributes url caption =
    link
        ([ paddingXY 35 15
         , Background.color primaryColorL
         , Border.rounded 4
         , Border.width 1
         , Border.solid
         , fonts
         , Font.size 14
         , Font.semiBold
         , Font.color secondaryColor
         , mouseOver [ Font.color secondaryColorD ]
         ]
            ++ attributes
        )
        { url = url, label = text (String.toUpper caption) }


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

sendGetUser : (Result Http.Error User -> msg) -> Int -> Session -> Cmd msg
sendGetUser responseMsg userId session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (Api.getUserById userId userInfo)
        Session.Guest _ ->
            Cmd.none

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

fonts =
    Font.family
        [ Font.typeface "-apple-system"
        , Font.typeface "BlinkMacSystemFont"
        , Font.typeface "Segoe UI"
        , Font.typeface "Roboto"
        , Font.typeface "Oxygen-Sans"
        , Font.typeface "Ubuntu"
        , Font.typeface "Cantarell"
        , Font.typeface "Helvetica Neue"
        , Font.sansSerif
        ]

primaryColorL =
    rgb255 255 255 255



secondaryColor =
    rgb255 96 125 139


secondaryColorD =
    rgb255 52 81 94