module UI.Elements exposing (..)


import Html exposing (Html, Attribute, div)
import Html.Attributes as Attributes exposing (class, classList, src)
import Html.Events as Events
import Json.Decode as Decode
import String.Extra exposing (toSentenceCase)

import Routing exposing (Route(..))
import Session exposing (Session, Notification)
import Api.Types exposing (Gender(..), Image)
import Api.Users exposing (User)

import Random


site : (a -> msg) -> List (Html a) -> Session -> List (Html msg)
site toMsg children session =
    [ toasts session
    , div [ class "main-wrapper" ]
        [ header session
        , content toMsg children
        , footer
        ]
    ]

toasts : Session -> Html msg
toasts session =
    div [ class "toasts" ]
        (List.map toast (Session.getNotifications session))


toast : Notification -> Html msg
toast notification =
    Html.p [ class "toast" ]
        [ Html.text notification ]

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
    div [ classList
            [ ( "l-6", True )
            , ( "s-8", True )
            ]
        ]
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
            [ ( "l-6", True )
            , ( "s-4", True )
            , ( "closed", True )
            ]
        ]
        [ Html.ul []
            (headerNavLinks session)
        ]


headerNavLinks : Session -> List (Html msg)
headerNavLinks session =
    case session of
        Session.LoggedIn _ _ userInfo ->
            [ headerNavLink (Routing.routeToString Messages) "Messages"
            , headerNavLink (Routing.routeToString ListUsers) "All users"
            , headerNavLink (Routing.routeToString (Profile userInfo.username)) "My profile"
            , headerNavLink (Routing.routeToString Logout) "Log out"
            ]

        Session.Guest _ _ ->
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


titledContent : String -> List (Html msg) -> List (Html msg)
titledContent heading contents =
    [ Html.h1 [ class "l-12"]
        [ Html.text heading ]
    ] ++ contents


titledContentLoader : Bool -> String -> List (Html msg) -> List (Html msg)
titledContentLoader isLoaded heading contents =
    if isLoaded then
        titledContent heading contents
    else
        titledContent heading
        loader

loader : List (Html msg)
loader =
    [ div [ class "loading-spinner" ] [] ]

userCard : User -> Html msg
userCard user =
    let
        username = user.username
        gender = Api.Types.genderToString user.gender
        age = "23"
        bio = user.profileText
    in
        Html.li
            [ classList [ ( "user-card", True )
                        , ( "l-4", True )
                        , ( "s-12", True )
                        ]
            ]
            [ Html.a [ class "profile-image",  Attributes.href (Routing.routeToString (Profile username)) ]
                [ div [ Attributes.style "background-image" ("url(" ++ user.image ++ ")") ] [] ]
            , div [ class "pri-title" ]
                [ Html.h2 []
                    [ Html.text (toSentenceCase username) ]
                , Html.h3 []
                    [ Html.text (gender ++ " - " ++ age) ]
                ]
            , Html.p []
                [ Html.text bio ]
            , linkButtonFlat
                [ classList
                    [ ("l-2", True)
                    , ("s-2", True)
                    ]
                ]
                (Routing.routeToString (Profile username))
                [ iconText "Profile" "perm_identity" ]
            , linkButtonFlat
                [ classList
                    [ ("l-2", True)
                    , ("s-2", True)
                    ]
                ]
                (Routing.routeToString (Chat username))
                [ iconText "Chat" "chat" ]
            ]


avatarUrl : User -> String
avatarUrl user =
    let
        genderString =
            case user.gender of
                Male ->
                    "men"
                Female ->
                    "women"
                Other ->
                    "lego"
        maxId =
            case user.gender of
                Male ->
                    100
                Female ->
                    100
                Other ->
                    9
    in
        "https://randomuser.me/api/portraits/" ++ genderString ++ "/" ++ user.username ++ ".jpg"


iconText : String -> String -> Html msg
iconText label iconName =
    div []
        [ materialIcon iconName
        , Html.text label
        ]

materialIcon : String -> Html msg
materialIcon iconName = 
    Html.i [ class "material-icons" ]
        [ Html.text iconName ]


textProperty : String -> String -> Html msg
textProperty labelText propertyText =
    div
        [ classList
            [ ( "property", True )
            , ( "l-6", True )
            ]
        ]
        [ Html.span []
            [ Html.text labelText ]
        , Html.span []
            [ Html.text propertyText ]
        ]

paragraphProperty : String -> String -> Html msg
paragraphProperty labelText propertyText =
    div
        [ classList
            [ ( "property", True )
            , ( "l-12", True )
            ]
        ]
        [ Html.span []
            [ Html.text labelText ]
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
            , Html.span
                [ classList
                    [ ( "errors", True )
                    , ( "hidden", not showErrors )
                    ]
                ]
                [ severestFieldError relevantErrors ]
            ]


severestFieldError : List ((fieldtype, String)) -> Html msg
severestFieldError errors =
    let
        maybeError = List.head errors
    in
        case maybeError of
            Nothing ->
                Html.text ""
            Just (_, errorDesc) ->
                Html.text errorDesc


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


imageInput : String -> msg -> Maybe Image -> Html msg
imageInput caption imageSelectedMsg maybeImage =
    let
        imagePreview =
            case maybeImage of
                Just image ->
                    imageElement image
                Nothing ->
                    Html.text ""
    in
        div
        [ classList
            [ ( "imageWrapper", True )
            , ( "l-6", True )
            , ("l-12", True )
            ]
        ]
        [ Html.input
            [ Attributes.type_ "file"
            , Events.on "change" (Decode.succeed imageSelectedMsg)
            ]
            []
        , imagePreview
        ]

imageElement : Image -> Html msg
imageElement image =
  Html.img
    [ Attributes.src image.contents
    , Attributes.title image.filename
    ]
    []


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

submitButtonHtml : List (Html msg) -> Html msg
submitButtonHtml children =
    Html.button
        [ Attributes.type_ "submit"
        , classList
            [ ( "btn", True )
            , ( "l-12", True )
            , ( "right", True )
            ]
        ]
        children
