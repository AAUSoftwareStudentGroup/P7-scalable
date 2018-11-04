module Page.Profile exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, div)

import Http
import String

import Api.Users exposing (User)
import Api.Types exposing (Gender(..))
import Routing exposing (Route(..))
import Session exposing (Session, Details)
import UI.Elements as El


type alias Model =
    { session   : Session
    , title     : String
    , user      : User
    }


type Msg
    = HandleGetUser (Result Http.Error User)
    | LogoutClicked


init : Session -> String -> ( Model, Cmd Msg )
init session username =
    ( Model session "Profile" Api.Users.emptyUser
    , sendGetUser HandleGetUser username session
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleGetUser result ->
            case result of
                Ok fetchedUser ->
                    ( { model | user = fetchedUser }, Cmd.none )

                Err errResponse ->
                    Debug.log (Debug.toString errResponse) ( { model | user = Api.Users.emptyUser }
                    , Routing.replaceUrl (Session.getNavKey model.session) (Routing.routeToString Home ) )

        LogoutClicked ->
            ( model, Session.logout )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Session.Details Msg
view model =
    { title = model.user.username ++ "'s profile"
    , session = model.session
    , kids =
        El.contentWithHeader (model.user.username ++ "'s profile")
            [ div []
                [ El.textProperty "Username" model.user.username
                , El.textProperty "Gender" (Api.Types.genderToString model.user.gender)
                , El.textProperty "Birthday" model.user.birthday
                , El.textProperty "Town" model.user.town
                , El.paragraphProperty "Description" model.user.profileText
                , chatButton model.user.username model.session
                ]
            ]
    }


chatButton : String -> Session -> Html msg
chatButton username session =
    El.linkButton
        []
        (Routing.routeToString <| (Chat username))
        [ Html.text "chat" ]



sendGetUser : (Result Http.Error User -> msg) -> String -> Session -> Cmd msg
sendGetUser responseMsg username session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (Api.Users.getUserByUsername username userInfo)

        Session.Guest _ ->
            Cmd.none
