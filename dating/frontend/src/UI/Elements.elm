module UI.Elements exposing (..)


import Html exposing (Html, div)
import Html.Attributes exposing (..)
import Html.Events as Events
import String.Extra exposing (toSentenceCase)

import Routing exposing (Route(..))
import Session exposing (Session)
import UI.Styles as Styles exposing (..)


site : (a -> msg) -> List (Html a) -> Session -> List (Html msg)
site toMsg children session =
    div []
        [ header session
        , content toMsg children
        , footer
        ]


header : Session -> Html msg
header session =
    div []
        [ headerLogo
        , headerNav session
        ]


headerLogo : Html msg
headerLogo =
    link headerLogoStyle (Routing.routeToString Home) "Dating"


headerNav : Session -> Html msg
headerNav session =
    row headerNavStyle (headerNavLinks session)


headerNavLinks : Session -> List (Html msg)
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


footer : Html msg
footer =
    row footerStyle [ el footerHtmlStyle (Html.text "A Dating Service that Rocks! © 2018") ]


content : (a -> msg) -> List (Html a) -> Html msg
content toMsg children =
    Html.map toMsg (column mainContentStyle children)


pageContent : String -> List (Html msg) -> List (Html msg)
pageContent heading contentHtmls =
    el contentHeadingStyle (Html.text heading) :: contentHtmls

contentColumn : Int -> List (Html msg) -> Html msg
contentColumn spacing children =
    column (contentColumnStyle spacing) children

userCard : String -> Int -> Int -> Html msg
userCard username userId friendId =
    el (fillStyle ++ cardStyle) <|
        row (fillStyle ++ [Html.spacing 16])
            [ Html.text (toSentenceCase username)
            , linkButtonRight (Routing.routeToString <| (Profile userId)) "Profile"
            , case userId == friendId of
                False ->
                    linkButtonRight (Routing.routeToString <| (Chat userId)) "chat"
                True ->
                    Html.none
            ]


placeholder : String -> Maybe (Placeholder msg)
placeholder caption =
    if caption == "" then
        Nothing
    else
        Just (Input.placeholder [] (Html.text caption))

formLabel : String -> Label msg
formLabel caption =
    Input.labelAbove formLabelStyle (Html.text caption)

textProperty : String -> String -> Html msg
textProperty labelText propertyText =
    column propertyStyle
        [ el propertyLabelStyle (Html.text labelText)
        , el propertyTextStyle (Html.text propertyText)
        ]


paragraphProperty : String -> String -> Html msg
paragraphProperty labelText propertyText =
    column propertyStyle
        [ el propertyLabelStyle (Html.text labelText)
        , Html.paragraph propertyTextStyle
            [ Html.text propertyText ]
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
    , label = Html.text (String.toUpper caption)
    }

warning : String -> Html msg
warning caption =
    el warningStyle (Html.text caption)

conditional : Html msg -> Bool -> Html msg
conditional element shouldShow =
        if shouldShow then
            element
        else
            Html.none

link : List (Attribute msg) -> String -> String -> Html msg
link styles url label =
    Html.link (linkStyle ++ styles) { url = url, label = Html.text label }
