module Routing exposing (Route(..), href, replaceUrl, goToLogin, pushUrl, routeToString)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)
import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr

-- ROUTING


type Route
    = Home
    | CreateUser
    | Login
    | Logout
    | ListUsers
    | Messages
    | Profile String
    | Chat String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map CreateUser (s "create-user")
        , Parser.map ListUsers (s "list-users")
        , Parser.map Messages (s "messages")
        , Parser.map Profile (s "user" </> Parser.string)
        , Parser.map Chat (s "chat" </> Parser.string)
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)

replaceUrl : Nav.Key -> String -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key route

goToLogin : Nav.Key -> Cmd msg
goToLogin key =
    replaceUrl key (routeToString Login)

pushUrl : Nav.Key -> String -> Cmd msg
pushUrl key route =
    Nav.pushUrl key route

-- INTERNAL


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []
                CreateUser ->
                    ["create-user"]

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                ListUsers ->
                    [ "list-users" ]
                
                Messages ->
                    [ "messages" ]

                Profile username ->
                    [ "user", username ]

                Chat username ->
                    [ "chat", username ]

    in
        "/" ++ String.join "/" pieces













