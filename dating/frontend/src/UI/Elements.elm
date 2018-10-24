module UI.Elements exposing (..)

import Element exposing (Attribute, Element, el, column, row)
import Element.Input as Input exposing (Placeholder, Label)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region
import String.Extra exposing (toSentenceCase)
import Html exposing (Html)

import Routing exposing (Route(..))
import Session exposing (Session)
import UI.Styles as Styles exposing (..)


site : (a -> msg) -> List (Element a) -> Session -> List (Html msg)
site toMsg children session =
    [ Element.layout siteStyle <|
        column fillStyle <|
            [ header session
            , content toMsg children
            , footer
            ]
    ]


header : Session -> Element msg
header session =
    row headerStyle
        [ headerLogo
        , headerNav session
        ]


headerLogo : Element msg
headerLogo =
    link headerLogoStyle (Routing.routeToString Home) "Dating"


headerNav : Session -> Element msg
headerNav session =
    row headerNavStyle (headerNavLinks session)


headerNavLinks : Session -> List (Element msg)
headerNavLinks session =
    case session of
        Session.LoggedIn _ userInfo ->
            [ link headerNavLinkStyle (Routing.routeToString Messages) "Messages"
            , link headerNavLinkStyle (Routing.routeToString ListUsers) "All users"
            , link headerNavLinkStyle (Routing.routeToString (Profile userInfo.userId)) "My profile"
            ]

        Session.Guest _ ->
            [ link headerNavLinkStyle (Routing.routeToString CreateUser) "Sign up"
            , link headerNavLinkStyle (Routing.routeToString Login) "Sign in"
            ]


footer : Element msg
footer =
    row footerStyle [ el footerElementStyle (Element.text "A Dating Service that Rocks! © 2018") ]


content : (a -> msg) -> List (Element a) -> Element msg
content toMsg children =
    Element.map toMsg (column mainContentStyle children)


pageContent : String -> List (Element msg) -> List (Element msg)
pageContent heading contentElements =
    el contentHeadingStyle (Element.text heading) :: contentElements

contentColumn : Int -> List (Element msg) -> Element msg
contentColumn spacing children =
    column (contentColumnStyle spacing) children

userCard : String -> Int -> Element msg
userCard username userId =
    el (fillStyle ++ cardStyle) <|
        row (fillStyle ++ [Element.spacing 16])
            [ Element.text (toSentenceCase username)
            , linkButtonRight (Routing.routeToString <| (Profile userId)) "Profile"
            , linkButtonRight (Routing.routeToString <| (Chat userId)) "chat"
            ]


placeholder : String -> Maybe (Placeholder msg)
placeholder caption =
    if caption == "" then
        Nothing
    else
        Just (Input.placeholder [] (Element.text caption))

formLabel : String -> Label msg
formLabel caption =
    Input.labelAbove formLabelStyle (Element.text caption)

textProperty : String -> String -> Element msg
textProperty labelText propertyText =
    column propertyStyle
        [ el propertyLabelStyle (Element.text labelText)
        , el propertyTextStyle (Element.text propertyText)
        ]


paragraphProperty : String -> String -> Element msg
paragraphProperty labelText propertyText =
    column propertyStyle
        [ el propertyLabelStyle (Element.text labelText)
        , Element.paragraph propertyTextStyle
            [ Element.text propertyText ]
        ]

linkButtonRight url caption =
    linkButton [ right ] url caption

linkButtonLeft url caption =
    linkButton [ right ] url caption

linkButton attributes url caption =
    link (buttonStyle ++ attributes) url (String.toUpper caption)

messageButtonRight msg caption =
    messageButton [ right ] msg caption

messageButtonLeft msg caption =
    messageButton [ right ] msg caption

messaageButtonCenter msg caption =
    messageButton centeredFillStyle msg caption

messageButton attributes msg caption =
    Input.button (buttonStyle ++ attributes)
    { onPress = Just msg
    , label = Element.text (String.toUpper caption)
    }

warning : String -> Element msg
warning caption =
    el warningStyle (Element.text caption)

conditional : Element msg -> Bool -> Element msg
conditional element shouldShow =
        if shouldShow then
            element
        else
            Element.none

link : List (Attribute msg) -> String -> String -> Element msg
link styles url label =
    Element.link (linkStyle ++ styles) { url = url, label = Element.text label }
