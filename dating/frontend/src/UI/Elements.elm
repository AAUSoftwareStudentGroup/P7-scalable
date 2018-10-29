module UI.Elements exposing (..)


import Html exposing (Html, Attribute, div)
import Html.Attributes as Attributes exposing (class, classList)
import Html.Events as Events
import String.Extra exposing (toSentenceCase)

import Routing exposing (Route(..))
import Session exposing (Session)


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
    Html.header []
        [ headerLogo
        , headerNav session
        ]


headerLogo : Html msg
headerLogo =
    div [ class "header-logo" ]
        [ link []
            (Routing.routeToString Home) "Dating"
        ]


headerNav : Session -> Html msg
headerNav session =
    Html.nav []
        (headerNavLinks session)


headerNavLinks : Session -> List (Html msg)
headerNavLinks session =
    let
        headerNavLinksClasses = []
    in
        case session of
            Session.LoggedIn _ userInfo ->
                [ link headerNavLinksClasses (Routing.routeToString Messages) "Messages"
                , link headerNavLinksClasses (Routing.routeToString ListUsers) "All users"
                , link headerNavLinksClasses (Routing.routeToString (Profile userInfo.userId)) "My profile"
                ]

            Session.Guest _ ->
                [ link headerNavLinksClasses (Routing.routeToString CreateUser) "Sign up"
                , link headerNavLinksClasses (Routing.routeToString Login) "Sign in"
                ]


footer : Html msg
footer =
    Html.footer []
        [Html.text "A Dating Service that Rocks! © 2018"]


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


userCard : String -> Int -> Int -> Html msg
userCard username userId friendId =
    Html.li []
        [ Html.text (toSentenceCase username)
        , linkButtonRight (Routing.routeToString (Profile userId)) "profile"
        , linkButtonRight (Routing.routeToString (Chat userId)) "chat"
        ]


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

linkButtonRight : String -> String -> Html msg
linkButtonRight url caption =
    linkButton [ class "right" ] url caption

linkButtonLeft : String -> String -> Html msg
linkButtonLeft url caption =
    linkButton [ class "left" ] url caption

linkButton : List (Attribute msg) -> String -> String -> Html msg
linkButton attributes url caption =
    link ([ class "button" ] ++ attributes) url caption

messageButtonRight : msg -> String -> Html msg
messageButtonRight msg caption =
    messageButton [ class "right" ] msg caption

messageButtonLeft : msg -> String -> Html msg
messageButtonLeft msg caption =
    messageButton [ class "left" ] msg caption

messageButton : List (Attribute msg) -> msg -> String -> Html msg
messageButton attributes msg caption =
    Html.input ([ class "button", Events.onClick msg] ++ attributes)
        [Html.text caption]


link : List (Attribute msg) -> String -> String -> Html msg
link attributes url label =
    Html.a ([Attributes.href url] ++ attributes) [Html.text label]


validatedInput : fieldType -> String -> String -> String -> (fieldType -> String -> msg) -> List ((fieldType, String)) -> Bool -> Html msg
validatedInput field typ caption value toMsg errors showErrors =
    let
        relevantErrors = List.filter (\( f, _ ) -> f == field) errors
    in
        div [ classList [("input-group", True), ("l-6", True), ( "valid", relevantErrors == [] )] ]
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
    div [ class "radio-group" ]
        [ Html.label []
            ([ Html.text caption ] ++ List.map (\(name, value) -> radio name toMsg model value) options)
        ]


radio : String -> (a -> msg) -> a -> a -> Html msg
radio caption toMsg model value =
    let
        id = caption ++ "-radio-input-id"
    in
        div [ class "radio-label-group" ]
            [ Html.label [Attributes.for id]
                [ Html.text caption ]
            , Html.input [ Attributes.type_ "radio", Attributes.id id, Attributes.checked (model == value), Events.onClick (toMsg value) ] []
            ]

simpleInput : String -> String -> String -> (String -> msg) -> Html msg
simpleInput typ placeholder value toMsg =
    if typ == "multiline" then
        Html.textarea [ Attributes.placeholder placeholder, Attributes.value value, Events.onInput toMsg ] []
    else
        Html.input [ Attributes.type_ typ, Attributes.placeholder placeholder, Attributes.value value, Events.onInput toMsg ] []


submitButton : String -> Html msg
submitButton caption =
    Html.button [Attributes.type_ "submit" ]
        [ Html.text caption ]
