module UI.Elements exposing (..)


import Html exposing (Html, Attribute, div)
import Html.Attributes as Attributes exposing (class, classList, src)
import Html.Events as Events
import String.Extra exposing (toSentenceCase)

import Routing exposing (Route(..))
import Session exposing (Session)

import Random


site : (a -> msg) -> List (Html a) -> Session -> List (Html msg)
site toMsg children session =
    [ div [class "main-wrapper" ]
        [ header session
        , content toMsg children
        , footer
        ]
    ]


header : Session -> Html msg
header session =
    Html.header [ class "grid" ]
        [ div
            [ classList
                    [ ( "header-content", True )
                    , ( "grid", True )
                    , ( "l-12", True )
                    ]
            ]
            [ headerLogo
            , headerNav session
            ]
        ]


headerLogo : Html msg
headerLogo =
    div [ class "l-6" ]
        [ Html.a [ class "logo", Attributes.href (Routing.routeToString Home) ]
            [ Html.i [ class "material-icons" ]
                [ Html.text "favorite" ]
            , Html.text "Dating"
            ]
        ]


headerNav : Session -> Html msg
headerNav session =
    Html.nav
        [ classList
            [ ( "l-6", True ) ]
        ]
        [ Html.ul []
            (headerNavLinks session)
        ]


headerNavLinks : Session -> List (Html msg)
headerNavLinks session =
    case session of
        Session.LoggedIn _ userInfo ->
            [ headerNavLink (Routing.routeToString Messages) "Messages"
            , headerNavLink (Routing.routeToString ListUsers) "All users"
            , headerNavLink (Routing.routeToString (Profile userInfo.userId)) "My profile"
            , headerNavLink (Routing.routeToString Logout) "Log out"
            ]

        Session.Guest _ ->
            [ headerNavLink (Routing.routeToString CreateUser) "Sign up"
            , headerNavLink (Routing.routeToString Login) "Sign in"
            ]

headerNavLink : String -> String -> Html msg
headerNavLink url caption =
    Html.li []
        [ link url caption ]

footer : Html msg
footer =
    Html.footer [ class "grid" ]
        [ div
            [ classList
                    [ ( "footer-content", True )
                    , ( "grid", True )
                    , ( "l-12", True )
                    ]
            ]
            [ div [ class "l-12" ]
                [ Html.text "A Dating Service that Rocks! © 2018" ]
            ]
        ]


content : (a -> msg) -> List (Html a) -> Html msg
content toMsg children =
    Html.map toMsg (
        div [ class "content-container", class "grid" ]
            children
    )


contentWithHeader : String -> List (Html msg) -> List (Html msg)
contentWithHeader heading contents =
    [ Html.h1 [ class "l-12"]
        [ Html.text heading ]
    ] ++ contents


userCard : String -> Int -> Html msg
userCard username userId =
    Html.li 
        [ classList [ ( "user-card", True )
                    , ( "l-12", True )
                    , ( "s-12", True )
                    , ( "grid", True )
                    ]
        ]
        [ Html.a [ Attributes.href (Routing.routeToString (Profile userId)) ]
                 [ Html.img [ src ("https://randomuser.me/api/portraits/men/"++(String.fromInt userId)++".jpg") ] [] ]
        , Html.span 
            [ classList [ ("l-7", True), ("s-7", True) ] ] 
            [ Html.text (toSentenceCase username) ]
        , linkButtonFlat
            [ classList [ ("l-2", True), ("s-2", True) ] ]
            (Routing.routeToString (Profile userId))
            [ labelledIcon "Profile" "perm_identity" 
            ]
        , linkButtonFlat
            [ classList [ ("l-2", True), ("s-2", True) ] ]
            (Routing.routeToString (Chat userId))
            [ Html.text "chat" ]
        ]

labelledIcon : String -> String -> Html msg
labelledIcon label iconName =
    div [ class "labelled-icon" ] 
    [ Html.span [] [ Html.text label ]
    , materialIcon iconName
    ]

materialIcon : String -> Html msg
materialIcon iconName = 
    Html.i [ class "material-icons" ] 
    [Html.text iconName ] 


textProperty : String -> String -> Html msg
textProperty labelText propertyText =
    div []
        [ Html.text labelText
        , Html.text propertyText
        ]


paragraphProperty : String -> String -> Html msg
paragraphProperty labelText propertyText =
    div []
        [ Html.text labelText
        , Html.p []
            [ Html.text propertyText ]
        ]


linkButton : List (Attribute msg) -> String -> List (Html msg) -> Html msg
linkButton attributes url children =
    Html.a ([ class "btn", Attributes.href url ] ++ attributes)
        children

linkButtonFlat : List (Attribute msg) -> String -> List (Html msg) -> Html msg
linkButtonFlat attributes url children =
    Html.a ([ class "flat-btn", Attributes.href url ] ++ attributes)
        children

msgButton : List (Attribute msg) -> msg -> List (Html msg) -> Html msg
msgButton attributes msg children =
    Html.a ([ class "btn", Events.onClick msg ] ++ attributes)
        children

msgButtonFlat : List (Attribute msg) -> msg -> List (Html msg) -> Html msg
msgButtonFlat attributes msg children =
    Html.a ([ class "flat-btn", Events.onClick msg ] ++ attributes)
        children

link : String -> String -> Html msg
link url caption =
    Html.a [ Attributes.href url ]
        [ Html.text caption ]


validatedInput : fieldType -> String -> String -> String -> (fieldType -> String -> msg) -> List ((fieldType, String)) -> Bool -> Html msg
validatedInput field typ caption value toMsg errors showErrors =
    let
        relevantErrors = List.filter (\( f, _ ) -> f == field) errors
    in
        div [ classList
                [ ( "input-group", True )
                , ( "l-6", True )
                , ( "s-12", True )
                , ( "valid", relevantErrors == [] )
                ]
            ]
            [ Html.label []
                [ simpleInput typ caption value (toMsg field)
                , Html.span [ class "label" ]
                    [ Html.text caption ]
                , Html.span [ class "border" ] []
                ]
            , Html.ul [ classList [("hidden", not showErrors)] ]
                (List.map fieldError relevantErrors)
            ]



fieldError : (fieldtype, String) -> Html msg
fieldError ( _, errorDesc) =
    Html.li []
        [ Html.text errorDesc ]


labelledRadio : String -> (a -> msg) -> a -> List (String, a) -> Html msg
labelledRadio caption toMsg model options =
    div [ classList
            [ ( "radio-group", True )
            , ( "l-6", True )
            , ( "s-12", True )
            ]
        ]
        ([ Html.label []
            [ Html.text caption ]
        ] ++ List.map (\(name, value) -> radio name toMsg model value) options)



radio : String -> (a -> msg) -> a -> a -> Html msg
radio caption toMsg model value =
    Html.label [ class "radio-label-group" ]
        [ Html.input [ Attributes.type_ "radio", Attributes.checked (model == value), Events.onClick (toMsg value) ]
            []
        , Html.span [ class "checkmark" ]
            []
        , Html.text caption
        ]


simpleInput : String -> String -> String -> (String -> msg) -> Html msg
simpleInput typ placeholder value toMsg =
    if typ == "multiline" then
        Html.textarea [ Attributes.placeholder placeholder, Attributes.value value, Events.onInput toMsg ] []
    else
        Html.input [ Attributes.type_ typ, Attributes.placeholder placeholder, Attributes.value value, Events.onInput toMsg ] []


submitButton : String -> Html msg
submitButton caption =
    Html.button
        [ Attributes.type_ "submit"
        , classList
            [ ( "btn", True )
            , ( "l-12", True )
            , ( "right", True )
            ]
        ]
        [ Html.text caption ]
