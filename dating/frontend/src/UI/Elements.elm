module UI.Elements exposing (asyncValidatedInput, avatarUrl, content, fixedContent, footer, header, headerLogo, headerNav, headerNavLink, headerNavLinks, iconText, imageElement, imageInput, labelledRadio, link, linkButton, linkButtonFlat, loader, materialIcon, modalBinary, modalMono, msgButton, msgButtonFlat, paragraphProperty, propertyGroup, radio, severestFieldError, simpleInput, site, submitButton, submitButtonHtml, textProperty, titledContent, titledContentLoader, toast, toasts, userCard, validatedInput)

import Api.Types exposing (Gender(..), Image)
import Api.Users exposing (Match, User)
import Date exposing (Date)
import Html exposing (Attribute, Html, div)
import Html.Attributes as Attributes exposing (class, classList, src)
import Html.Events as Events
import Html.Lazy as Lazy
import Json.Decode as Decode
import Random
import Routing exposing (Route(..))
import Session exposing (Details, Notification, PageType(..), Session)
import String.Extra exposing (toSentenceCase)
import Time


site : Session.Details msgA -> (msgA -> msgB) -> List (Html msgB)
site details toMsg =
    let
        session =
            details.session

        body =
            case details.kids of
                Scrollable children ->
                    Lazy.lazy2 content toMsg children

                Fixed children ->
                    Lazy.lazy2 fixedContent toMsg children
    in
    [ Lazy.lazy toasts session
    , div [ class "main-wrapper" ]
        [ Lazy.lazy header session
        , body
        , footer session
        ]
    ]


toasts : Session -> Html msg
toasts session =
    div [ class "toasts" ]
        (List.map toast (Session.getNotifications session))


toast : Notification -> Html msg
toast notification =
    Html.p [ class "toast" ]
        [ Html.text notification.body ]


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
    div
        [ classList
            [ ( "l-6", True )
            , ( "s-8", True )
            ]
        ]
        [ Html.a [ class "logo", Attributes.href (Routing.routeToString Home) ]
            [ Html.i [ class "material-icons" ]
                [ Html.text "people" ]
            , Html.text "friendr"
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
        Session.LoggedIn _ _ _ userInfo ->
            [ headerNavLink (Routing.routeToString ListUsers) "Matches"
            , headerNavLink (Routing.routeToString (Messages "")) "Messages"
            , headerNavLink (Routing.routeToString (Profile userInfo.username)) "Profile"
            , headerNavLink (Routing.routeToString Logout) "Log out"
            ]

        Session.Guest _ _ _ ->
            [ headerNavLink (Routing.routeToString CreateUser) "Sign up"
            , headerNavLink (Routing.routeToString Login) "Sign in"
            ]


headerNavLink : String -> String -> Html msg
headerNavLink url caption =
    Html.li []
        [ link url caption ]


footer : Session -> Html msg
footer session =
    let
        dateNow =
            Date.fromPosix Time.utc <| Session.getNow session

        year =
            Date.year dateNow
    in
    Html.footer [ class "grid" ]
        [ div
            [ classList
                [ ( "footer-content", True )
                , ( "grid", True )
                , ( "l-12", True )
                ]
            ]
            [ div [ class "l-12" ]
                [ Html.text <| "A people matching service that is also functional! © " ++ String.fromInt year ]
            ]
        ]


content : (a -> msg) -> List (Html a) -> Html msg
content toMsg children =
    Html.map toMsg
        (div [ class "content-container", class "grid" ]
            children
        )


fixedContent : (a -> msg) -> List (Html a) -> Html msg
fixedContent toMsg children =
    Html.map toMsg
        (div
            [ classList
                [ ( "content-container", True )
                , ( "fixed", True )
                , ( "grid", True )
                ]
            ]
            children
        )


titledContent : String -> List (Html msg) -> List (Html msg)
titledContent heading contents =
    [ Html.h1
        [ classList
            [ ( "content-title", True )
            , ( "l-12", True )
            , ( "s-12", True )
            ]
        ]
        [ Html.text heading ]
    ]
        ++ contents


titledContentLoader : Bool -> String -> List (Html msg) -> List (Html msg)
titledContentLoader isLoaded heading contents =
    if isLoaded then
        titledContent heading contents

    else
        titledContent heading loader


loader : List (Html msg)
loader =
    [ div [ class "loading-spinner" ] [] ]


userCard : Match -> Int -> Html msg
userCard match age =
    let
        username =
            match.user.username

        gender =
            Api.Types.genderToString match.user.gender

        bio =
            match.user.profileText
    in
    Html.li
        [ classList
            [ ( "user-card", True )
            , ( "l-4", True )
            , ( "s-12", True )
            ]
        ]
        [ Html.a [ class "profile-image", Attributes.href (Routing.routeToString (Profile username)) ]
            [ div [ Attributes.style "background-image" ("url(" ++ match.user.image ++ ")") ] [] ]
        , div [ class "pri-title" ]
            [ Html.h2 []
                [ Html.text (toSentenceCase username) ]
            , Html.h3 []
                [ Html.text (gender ++ " - " ++ String.fromInt age) ]
            ]
        , Html.p []
            [ Html.text bio ]
        , linkButtonFlat
            [ classList
                [ ( "l-2", True )
                , ( "s-2", True )
                ]
            ]
            (Routing.routeToString (Profile username))
            [ iconText "Profile" "perm_identity" ]
        , linkButtonFlat
            [ classList
                [ ( "l-2", True )
                , ( "s-2", True )
                ]
            ]
            (Routing.routeToString (Messages username))
            [ iconText "Chat" "chat" ]
        ,Html.span [ class "match-score" ]
            [ Html.text <| String.fromInt (round match.score) ]
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


propertyGroup : String -> String -> Html msg
propertyGroup label value =
    div
        [ classList
            [ ( "property-group", True )
            , ( "l-6", True )
            , ( "s-12", True )
            ]
        ]
        [ Html.span [ class "label" ]
            [ Html.text label ]
        , Html.span [ class "value" ]
            [ Html.text value ]
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
    Html.div ([ class "btn", Events.onClick msg ] ++ attributes)
        children


msgButtonFlat : List (Attribute msg) -> msg -> List (Html msg) -> Html msg
msgButtonFlat attributes msg children =
    Html.div ([ class "flat-btn", Events.onClick msg ] ++ attributes)
        children


link : String -> String -> Html msg
link url caption =
    Html.a [ Attributes.href url ]
        [ Html.text caption ]


validatedInput : fieldType -> String -> String -> String -> (fieldType -> String -> msg) -> Bool -> List ( fieldType, String ) -> Bool -> Html msg
validatedInput field typ caption value toMsg required errors showErrors =
    let
        relevantErrors =
            List.filter (\( f, _ ) -> f == field) errors

        empty =
            value == ""

        valid =
            relevantErrors == []
    in
    div
        [ classList
            [ ( "input-group", True )
            , ( "l-6", True )
            , ( "s-12", True )
            , ( "empty", empty )
            , ( "valid", valid )
            ]
        ]
        [ Html.label []
            [ simpleInput typ caption value (toMsg field) required
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


asyncValidatedInput : fieldType -> String -> String -> String -> (fieldType -> String -> msg) -> Bool -> List ( fieldType, String ) -> Bool -> Bool -> Html msg
asyncValidatedInput field typ caption value toMsg required errors showErrors pending =
    let
        relevantErrors =
            List.filter (\( f, _ ) -> f == field) errors

        empty =
            value == ""

        valid =
            relevantErrors == []
    in
    div
        [ classList
            [ ( "input-group", True )
            , ( "l-6", True )
            , ( "s-12", True )
            , ( "empty", empty )
            , ( "valid", valid )
            , ( "pending", pending )
            ]
        ]
        [ Html.label []
            [ simpleInput typ caption value (toMsg field) required
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


severestFieldError : List ( fieldtype, String ) -> Html msg
severestFieldError errors =
    let
        maybeError =
            List.head errors
    in
    case maybeError of
        Nothing ->
            Html.text ""

        Just ( _, errorDesc ) ->
            Html.text errorDesc


labelledRadio : String -> (a -> msg) -> a -> List ( String, a ) -> Html msg
labelledRadio caption toMsg modelVal options =
    div
        [ classList
            [ ( "radio-group", True )
            , ( "l-6", True )
            , ( "s-12", True )
            ]
        ]
        ([ Html.label []
            [ Html.text caption ]
         ]
            ++ List.map (\( name, value ) -> radio name toMsg modelVal value) options
        )


radio : String -> (a -> msg) -> a -> a -> Html msg
radio caption toMsg modelVal value =
    Html.label [ class "radio-label-group" ]
        [ Html.input [ Attributes.type_ "radio", Attributes.checked (modelVal == value), Events.onClick (toMsg value) ]
            []
        , Html.span [ class "checkmark" ]
            []
        , Html.text caption
        ]


simpleInput : String -> String -> String -> (String -> msg) -> Bool -> Html msg
simpleInput typ placeholder value toMsg required =
    if typ == "multiline" then
        Html.textarea [ Attributes.placeholder placeholder, Attributes.value value, Events.onInput toMsg ] []

    else
        Html.input [ Attributes.type_ typ, Attributes.placeholder placeholder, Attributes.value value, Attributes.required required, Events.onInput toMsg ] []


imageInput : String -> msg -> Maybe Image -> Html msg
imageInput caption imageSelectedMsg maybeImage =
    let
        noImage =
            case maybeImage of
                Just image ->
                    False

                Nothing ->
                    True

        imageURI =
            case maybeImage of
                Just image ->
                    "url(" ++ image.contents ++ ")"

                Nothing ->
                    ""
    in
    div
        [ classList
            [ ( "image-input-group", True )
            , ( "no-image", noImage )
            , ( "l-4", True )
            , ( "s-8", True )
            ]
        , Attributes.style "background-image" imageURI
        ]
        [ div []
            [ Html.label [ class "btn" ]
                [ iconText "Choose a file" "image"
                , Html.input
                    [ Attributes.type_ "file"
                    , Attributes.accept "image/*"
                    , Events.on "change" (Decode.succeed imageSelectedMsg)
                    ]
                    []
                ]
            ]
        ]


imageElement : Image -> List ( String, Bool ) -> Html msg
imageElement image classes =
    Html.img
        [ Attributes.src image.contents
        , Attributes.title image.filename
        , classList classes
        ]
        []


submitButton : List ( String, Bool ) -> String -> Html msg
submitButton classes caption =
    Html.button
        [ Attributes.type_ "submit"
        , classList
            ([ ( "btn", True )
             ]
                ++ classes
            )
        ]
        [ Html.text caption ]


submitButtonHtml : List ( String, Bool ) -> List (Html msg) -> Html msg
submitButtonHtml classes children =
    Html.button
        [ Attributes.type_ "submit"
        , classList
            ([ ( "btn", True )
             ]
                ++ classes
            )
        ]
        children


modalMono : String -> String -> msg -> Html msg
modalMono caption btnText toMsg =
    div [ class "modal" ]
        [ Html.p []
            [ Html.text caption ]
        , Html.button
            [ Events.onClick toMsg
            , class "btn"
            , class "accept"
            ]
            [ Html.text btnText ]
        ]


modalBinary : String -> String -> msg -> String -> msg -> Html msg
modalBinary caption accBtnText accMsg decBtnText decMsg =
    div [ class "modal" ]
        [ Html.p []
            [ Html.text caption ]
        , Html.button
            [ Events.onClick accMsg
            , class "btn"
            , class "accept"
            ]
            [ Html.text accBtnText ]
        , Html.button
            [ Events.onClick decMsg
            , class "btn"
            , class "decline"
            ]
            [ Html.text decBtnText ]
        ]
