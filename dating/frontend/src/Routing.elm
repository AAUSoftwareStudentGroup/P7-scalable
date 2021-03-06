module Routing exposing (Route(..), goToLogin, href, pushUrl, replaceUrl, routeToString)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, oneOf, s)



-- ROUTING


type Route
    = Home
    | CreateUser
    | Login
    | Logout
    | ListUsers
    | Messages String
    | EditUser
    | Profile String
    | Survey


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map CreateUser (s "create-user")
        , Parser.map ListUsers (s "matches")
        , Parser.map Messages (s "messages" </> Parser.string)
        , Parser.map EditUser (s "edit")
        , Parser.map Profile (s "user" </> Parser.string)
        , Parser.map Survey (s "survey")
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
                    [ "create-user" ]

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                ListUsers ->
                    [ "matches" ]

                Messages username ->
                    [ "messages", username ]

                EditUser ->
                    [ "edit" ]

                Profile username ->
                    [ "user", username ]

                Survey ->
                    [ "survey" ]
    in
    "/" ++ String.join "/" pieces
