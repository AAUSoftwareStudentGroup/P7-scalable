module UI.Elements exposing (..)


import Html exposing (Html, Attribute, div)
import Html.Attributes as Attributes exposing (class)
import Html.Events as Events
import String.Extra exposing (toSentenceCase)

import Routing exposing (Route(..))
import Session exposing (Session)


site : (a -> msg) -> List (Html a) -> Session -> List (Html msg)
site toMsg children session =
    [ header session
    , content toMsg children
    , footer
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
        div [ class "content-container" ]
            [ div [ class "content" ]
                children
            ]
    )


contentWithHeader : String -> List (Html msg) -> List (Html msg)
contentWithHeader heading contents =
    [ Html.h1 []
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

warning : String -> Html msg
warning caption =
    Html.span []
        [Html.text caption]

conditional : Html msg -> Bool -> Html msg
conditional element shouldShow =
    element

link : List (Attribute msg) -> String -> String -> Html msg
link attributes url label =
    Html.a ([Attributes.href url] ++ attributes) [Html.text label]

validatedInput : fieldType -> String -> String -> String -> String -> (fieldType -> String -> msg) -> List ((fieldType, String)) ->  Html msg
validatedInput field typ caption placeholder value toMsg errors =
    div []
        [ Html.label []
            [ Html.text caption
            , simpleInput typ placeholder value (toMsg field)
            ]
        , fieldErrors field errors
        ]

fieldErrors : a -> List ((a, String)) -> Html msg
fieldErrors filterField errors =
    Html.ul []
        (List.map fieldError (List.filter (\( field, _ ) -> field == filterField) errors))


fieldError : (a, String) -> Html msg
fieldError ( _, errorDesc) =
    Html.li []
        [ Html.text errorDesc ]

labeledInput : String -> String -> String -> String -> (String -> msg) -> Html msg
labeledInput typ caption placeholder value toMsg =
    div []
        [ Html.label []
            [ Html.text caption
            , simpleInput typ placeholder value toMsg
            ]
        ]

labelledRadio : String -> (a -> msg) -> a -> List (String, a) -> Html msg
labelledRadio caption toMsg model options =
    div []
        [ Html.label []
            ([ Html.text caption ] ++ List.map (\(name, value) -> radio name toMsg model value) options)
        ]


radio : String -> (a -> msg) -> a -> a -> Html msg
radio caption toMsg model value =
    Html.label []
      [ Html.input [ Attributes.type_ "radio", Attributes.checked (model == value), Events.onClick (toMsg value) ] []
      , Html.text caption
      ]

simpleInput : String -> String -> String -> (String -> msg) -> Html msg
simpleInput typ placeholder value toMsg  =
    if typ == "multiline" then
        Html.textarea [ Attributes.placeholder placeholder, Attributes.value value, Events.onInput toMsg ] []
    else
        Html.input [ Attributes.type_ typ, Attributes.placeholder placeholder, Attributes.value value, Events.onInput toMsg ] []


submitButton : String -> Html msg
submitButton caption =
    Html.button [Attributes.type_ "submit" ]
        [ Html.text caption ]
