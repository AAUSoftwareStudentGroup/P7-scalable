module Page.Profile exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Navigation as Nav
import DatingApi as Api exposing (Gender(..), User, emptyUser, genderToString)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Http
import String

import DatingApi as Api exposing (Gender(..), User)
import Routing exposing (Route(..))
import Session exposing (Session, Details)
import UI.Elements as El
import UI.Styles exposing (centeredFillStyle)


type alias Model =
    { session : Session
    , title : String
    , id : Int
    , user : User
    }


type Msg
    = HandleGetUser (Result Http.Error User)
    | LogoutClicked
    | SessionChanged Session


init : Session -> Int -> ( Model, Cmd Msg )
init session id =
    ( Model session "Profile" id emptyUser
    , sendGetUser HandleGetUser id session
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleGetUser result ->
            case result of
                Ok fetchedUser ->
                    ( { model | user = fetchedUser }, Cmd.none )

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

view : Model -> Session.Details Msg
view model =
    { title = model.user.userUsername ++ "'s profile"
    , session = model.session
    , kids =
        El.pageContent (model.user.userUsername ++ "'s profile") <|
            [ Element.row centeredFillStyle <|
                [ El.formColumn
                    [ El.textProperty "Username" model.user.userUsername
                    , El.textProperty "Email" model.user.userEmail
                    , El.textProperty "Gender" (genderToString model.user.userGender)
                    , El.textProperty "Birthday" model.user.userBirthday
                    , El.textProperty "Town" model.user.userTown
                    , El.paragraphProperty "Description" model.user.userProfileText
                    ]
                , chatButton model.user.userId model.session
                ]

            ]
    }


chatButton : Int -> Session -> Element msg
chatButton friendId session =
    case Session.getUserId session of
        Just userId ->
            case userId == friendId of
                False ->
                    El.linkButtonRight (Routing.routeToString <| (Chat friendId)) "chat"
                True ->
                    Element.none
        Nothing ->
            Element.none


sendGetUser : (Result Http.Error User -> msg) -> Int -> Session -> Cmd msg
sendGetUser responseMsg userId session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (Api.getUserById userId userInfo)

        Session.Guest _ ->
            Cmd.none
