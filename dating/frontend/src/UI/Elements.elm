module UI.Elements exposing (content, footer, header, headerLogo, headerNav, headerNavLinks, link, site)

import Element exposing (Attribute, Element, column, el, layout, row)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import Html exposing (Html)
import Routing exposing (Route(..))
import Session exposing (Session)
import UI.Styles as Styles exposing (..)


site : (a -> msg) -> List (Element a) -> Session -> List (Html msg)
site toMsg children session =
    [ layout siteStyles <|
        column fillStyles <|
            [ header session
            , content toMsg children
            , footer
            ]
    ]


header : Session -> Element msg
header session =
    row headerStyles
        [ headerLogo
        , headerNav session
        ]


headerLogo : Element msg
headerLogo =
    link headerLogoStyles (Routing.routeToString Home) "Dating"


headerNav : Session -> Element msg
headerNav session =
    row headerNavStyles (headerNavLinks session)


headerNavLinks : Session -> List (Element msg)
headerNavLinks session =
    case session of
        Session.LoggedIn _ userInfo ->
            [ link headerNavLinkStyles (Routing.routeToString Messages) "Messages"
            , link headerNavLinkStyles (Routing.routeToString ListUsers) "All users"
            , link headerNavLinkStyles (Routing.routeToString (Profile userInfo.userId)) "My profile"
            ]

        Session.Guest _ ->
            [ link headerNavLinkStyles (Routing.routeToString CreateUser) "Sign up"
            , link headerNavLinkStyles (Routing.routeToString Login) "Sign in"
            ]


footer : Element msg
footer =
    row footerStyles [ el footerElementStyle (Element.text "A Dating Service that Rocks! © 2018") ]


content : (a -> msg) -> List (Element a) -> Element msg
content toMsg children =
    Element.map toMsg (column contentStyle children)


link : List (Attribute msg) -> String -> String -> Element msg
link styles url label =
    Element.link (linkStyles ++ styles) { url = url, label = Element.text label }
