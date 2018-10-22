module Page.ListUsers exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import String exposing (toUpper)
import String.Extra exposing (toSentenceCase)
import List exposing (concat)
import Url

import DatingApi as Api exposing (User)
import Session exposing (Session, Details)
import Routing exposing (Route(..))


-- MODEL


type alias Model =
    { session : Session
    , title : String
    , users : List User
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Model session "List Users" []
    , sendGetUsers UsersFetched session
    )



-- UPDATE


type Msg
    = UsersFetched (Result Http.Error (List User))
    | SessionChanged Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UsersFetched result ->
            case result of
                Ok newUsers ->
                    ( { model | users = newUsers }, Cmd.none )

                Err error ->
                    Debug.log (Debug.toString error) ( { model | users = [] }, Cmd.none )
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



-- VIEW


view : Model -> Session.Details Msg
view model =
    { title = "All users"
    , session = model.session
    , kids =
        [ column [ width (px 600), height shrink, centerY, centerX, spacing 36, padding 10 ]
            (el
                [ Region.heading 1
                , centerX
                , Font.size 36
                ]
                (text "Users")
                :: List.map showUser model.users
            )
        ]
    }


showUser : User -> Element Msg
showUser user =
    el (concat [ [ width fill, mouseOver profileShadowHover ], profileShadow ]) <|
        row [ spacing 10, padding 20, width fill]
            [ createLink (Routing.routeToString <| (Profile user.userId))
                [width fill, height fill]
                (el [ Font.size 24, alignLeft ] <| text <| toSentenceCase <| user.userUsername)
            , createButtonRight (Routing.routeToString <| (Chat user.userId)) "chat"
            ]


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
        { url = url, label = text (toUpper caption) }


createLink : String -> List (Attribute Msg) -> Element Msg -> Element Msg
createLink url attributes innerHtml =
    link attributes { url = url, label = innerHtml }


profileShadow =
    [ Border.shadow profileShadowA
    , Border.shadow profileShadowB
    , Border.shadow profileShadowC
    ]


profileShadowA =
    { offset = ( 0.0, 2.0 )
    , size = 0
    , blur = 2.0
    , color = rgba 0 0 0 0.14
    }


profileShadowB =
    { offset = ( 0.0, 3.0 )
    , size = -2
    , blur = 1.0
    , color = rgba 0 0 0 0.12
    }


profileShadowC =
    { offset = ( 0.0, 1.0 )
    , size = 0
    , blur = 5.0
    , color = rgba 0 0 0 0.2
    }


profileShadowHover =
    [ Border.shadow profileShadowHoverA
    , Border.shadow profileShadowHoverB
    ]


profileShadowHoverA =
    { offset = ( 0.0, 8.0 )
    , size = 0
    , blur = 17
    , color = rgba 0 0 0 0.2
    }


profileShadowHoverB =
    { offset = ( 0.0, 6.0 )
    , size = 0
    , blur = 20
    , color = rgba 0 0 0 0.19
    }


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


primaryColor =
    rgb255 207 216 220


primaryColorL =
    rgb255 255 255 255


primaryColorD =
    rgb255 158 167 170


secondaryColor =
    rgb255 96 125 139


secondaryColorL =
    rgb255 142 172 187


secondaryColorD =
    rgb255 52 81 94


textColor =
    rgb255 0 0 0

edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }

sendGetUsers : (Result Http.Error (List User) -> msg) -> Session -> Cmd msg
sendGetUsers responseMsg session =
    case session of
        Session.LoggedIn _ userInfo ->
            Http.send responseMsg (Api.getUsers userInfo)
        Session.Guest _ ->
            Cmd.none
