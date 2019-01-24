module Page.Profile exposing (Model, Msg(..), init, update, view)

import Api.Types exposing (Gender(..), dateToString)
import Api.Users exposing (User, emptyUser)
import Browser.Navigation as Nav
import Html exposing (Html, div)
import Html.Attributes exposing (classList, src, style, title)
import Http
import Routing exposing (Route(..))
import Session exposing (Details, PageType(..), Session)
import String
import UI.Elements as El


type alias Model =
    { session : Session
    , title   : String
    , loaded  : Bool
    , user    : User
    }


emptyModel : Session -> Model
emptyModel session =
    Model session "Profile" False emptyUser


type Msg
    = HandleGetUser (Result Http.Error User)


init : Session -> String -> ( Model, Cmd Msg )
init session username =
    case session of
        Session.Guest _ _ _ ->
            ( emptyModel session
            , Routing.goToLogin (Session.getNavKey session)
            )

        Session.LoggedIn _ _ _ _ ->
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
                    case errResponse of
                        Http.BadStatus response ->
                            if response.status.code == 403 then
                                ( model, Session.logout )

                            else
                                ( { model | session = Session.addNotification model.session ("Error: " ++ .body response) }, Cmd.none )

                        _ ->
                            ( { model | session = Session.addNotification model.session "Error: Something went wrong" }, Cmd.none )


view : Model -> Session.Details Msg
view model =
    { title = model.title
    , session = model.session
    , kids =
        Scrollable <|
            El.titledContentLoader model.loaded
                model.user.username
                [ div
                    [ classList
                        [ ( "grid", True )
                        , ( "l-12", True )
                        ]
                    ]
                    [ showImg model.user
                    , El.textProperty "Gender" (Api.Types.genderToString model.user.gender)
                    , El.textProperty "Birthday" <| dateToString model.user.birthday
                    , El.textProperty "Town" model.user.town
                    , El.paragraphProperty "Description" model.user.profileText
                    , chatButton model.user.username model.session
                    , editButton model.user.username model.session
                    , surveyButton model.user.username model.session
                    ]
                ]
    }


showImg : User -> Html msg
showImg user =
    Html.img
        [ src user.image
        , classList
            [ ( "profile-image", True )
            , ( "s-6", True )
            , ( "l-12", True )
            ]
        []


chatButton : String -> Session -> Html msg
chatButton username session =
    if Just username == Session.getUsername session then
        Html.text ""

    else
        El.linkButton
            []
            (Routing.routeToString (Messages username))
            [ Html.text "chat" ]


editButton : String -> Session -> Html msg
editButton username session =
    if Just username == Session.getUsername session then
        El.linkButton
            []
            (Routing.routeToString EditUser)
            [ Html.text "Edit" ]

    else
        Html.text ""


surveyButton : String -> Session -> Html msg
surveyButton username session =
    if Just username == Session.getUsername session then
        El.linkButton
            []
            (Routing.routeToString Survey)
            [ Html.text "Survey" ]

    else
        Html.text ""


sendGetUser : (Result Http.Error User -> msg) -> String -> Session -> Cmd msg
sendGetUser responseMsg username session =
    case session of
        Session.LoggedIn _ _ _ userInfo ->
            Http.send responseMsg (Api.Users.getUserByUsername userInfo username)

        Session.Guest _ _ _ ->
            Cmd.none
