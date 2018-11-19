module Page.Profile exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, div)
import Html.Attributes exposing (classList, src, title, style)

import Http
import String

import Api.Users exposing (User, emptyUser)
import Api.Types exposing (Gender(..))
import Routing exposing (Route(..))
import Session exposing (Session, Details)
import UI.Elements as El


type alias Model =
    { session   : Session
    , title     : String
    , loaded    : Bool
    , user      : User
    }


emptyModel : Session -> Model
emptyModel session =
    Model session "Profile" False emptyUser


type Msg
    = HandleGetUser (Result Http.Error User)


init : Session -> String -> ( Model, Cmd Msg )
init session username =
    case session of
        Session.Guest _ _ ->
            ( emptyModel session
            , Routing.goToLogin (Session.getNavKey session)
            )
        Session.LoggedIn _ _ _ ->
            ( emptyModel session
            , sendGetUser HandleGetUser username session
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleGetUser result ->
            case result of
                Ok fetchedUser ->
                    ( { model | user = fetchedUser, loaded = True }, Cmd.none )
                Err errResponse ->
                    Debug.log (Debug.toString errResponse) ( { model | user = Api.Users.emptyUser }
                    , Routing.replaceUrl (Session.getNavKey model.session) (Routing.routeToString Home ) )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        El.titledContentLoader model.loaded model.user.username
            [ div
                [ classList
                    [ ( "grid", True )
                    , ( "l-12", True )
                    , ( "s-6", True )
                    ]
                ]
                [ showImg model.user
                , El.textProperty "Gender" (Api.Types.genderToString model.user.gender)
                , El.textProperty "Birthday" model.user.birthday
                , El.textProperty "Town" model.user.town
                , El.paragraphProperty "Description" model.user.profileText
                , chatButton model.user.username model.session
                ]
            ]
    }


showImg : User -> Html msg
showImg user =
  Html.img
    [ src user.image
    , style "width" "100px"
    , style "height" "100px"
    ]
    []


chatButton : String -> Session -> Html msg
chatButton username session =
    El.linkButton
        []
        (Routing.routeToString (Chat username))
        [ Html.text "chat" ]


sendGetUser : (Result Http.Error User -> msg) -> String -> Session -> Cmd msg
sendGetUser responseMsg username session =
    case session of
        Session.LoggedIn _ _ userInfo ->
            Http.send responseMsg (Api.Users.getUserByUsername username userInfo)

        Session.Guest _ _ ->
            Cmd.none
