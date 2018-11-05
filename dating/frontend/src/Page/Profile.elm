module Page.Profile exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Navigation as Nav
import DatingApi as Api exposing (Gender(..), User, emptyUser, genderToString)
import Html exposing (Html, div)
import Html.Attributes exposing (classList)

import Http
import String

import DatingApi as Api exposing (Gender(..), User)
import Routing exposing (Route(..))
import Session exposing (Session, Details)
import UI.Elements as El


type alias Model =
    { session : Session
    , title : String
    , id : Int
    , user : User
    }


type Msg
    = HandleGetUser (Result Http.Error User)
    | LogoutClicked


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
                    Debug.log (Debug.toString errResponse) ( { model | user = emptyUser }
                    , Routing.replaceUrl (Session.getNavKey model.session) (Routing.routeToString Home ) )

        LogoutClicked ->
            ( model, Session.logout )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Session.Details Msg
view model =
    { title = model.user.userUsername ++ "'s profile"
    , session = model.session
    , kids =
        El.contentWithHeader model.user.userUsername
            [ div
                [ classList
                    [ ( "grid", True )
                    , ( "l-12", True )
                    , ( "s-6", True )
                    ]
                ]
                [ El.textProperty "Email" model.user.userEmail
                , El.textProperty "Gender" (genderToString model.user.userGender)
                , El.textProperty "Birthday" model.user.userBirthday
                , El.textProperty "Town" model.user.userTown
                , El.paragraphProperty "Description" model.user.userProfileText
                , chatButton model.user.userId model.session
                ]
            ]
    }


chatButton : Int -> Session -> Html msg
chatButton friendId session =
    El.linkButton
        []
        (Routing.routeToString <| (Chat friendId))
        [ Html.text "chat" ]



sendGetUser : (Result Http.Error User -> msg) -> Int -> Session -> Cmd msg
sendGetUser responseMsg userId session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (Api.getUserById userId userInfo)

        Session.Guest _ ->
            Cmd.none
