module Skeleton exposing (Details, view)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Routing exposing (Route(..))
import Session exposing (Session)

type alias Details msg =
    { title : String
    , session: Session
    , kids : List (Element msg)
    }

view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    { title =
        details.title
    , body =
        [ layout
            [ spacing 0
            , centerX
            ]
          <|
            column [ width fill, height fill ] <|
                [ viewHeader details
                , Element.map toMsg <| column [ centerX, padding 30, Background.color white ] <| details.kids
                , viewFooter
                ]
        ]
    }


viewHeader : Details a -> Element msg
viewHeader details =
    row
        [ padding 5
        , width fill
        , Font.color white
        , Background.color red
        ]
        [ el
            [ Region.heading 1, paddingXY 40 10, Font.size 40 ]
            (text "Dating")
        , row [ alignRight, spacingXY 60 20, padding 40 ] <|
            [ link linkStyle { url = Routing.routeToString CreateUser, label = text "Create User" }
            , link linkStyle { url = Routing.routeToString Messages, label = text "Messages" }
            , accountLink details.session
            ]
        ]

accountLink : Session -> Element msg
accountLink session =
    case session of
        Session.LoggedIn _ _ ->
            link linkStyle { url = Routing.routeToString Login, label = text "Logout" }

        Session.Guest key ->
            link linkStyle { url = Routing.routeToString Login, label = text "Login" }


viewFooter : Element msg
viewFooter =
    row
        [ width fill
        , padding 5
        , Font.color white
        , alignBottom
        , Background.color red
        ]
        [ el [ Region.footer, centerX ] (text "A Dating Service that Rocks! © 2018") ]


linkStyle =
    [ Font.color white, Font.heavy, Font.underline]


black =
    rgb 0 0 0


red =
    rgb255 255 105 97


cyan =
    rgb255 75 165 180


white =
    rgb 1 1 1


gray =
    Element.rgb 0.5 0.5 0.5
