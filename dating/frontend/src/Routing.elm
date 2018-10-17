module Routing exposing (Route(..), fromUrl, href, replaceUrl, pushUrl, routeToString)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string, int)
import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr

-- ROUTING


type Route
    = Home
    | CreateUser
    | Login
    | ListUsers
    | Messages
    | Profile Int


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map CreateUser (s "create-user")
        , Parser.map ListUsers (s "list-users")  
        , Parser.map Messages (s "messages")
        , Parser.map Profile (s "user" </> int)
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> String -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key route

pushUrl : Nav.Key -> String -> Cmd msg
pushUrl key route =
    Nav.pushUrl key route

fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



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

                ListUsers ->
                    [ "list-users" ]
                
                Messages ->
                    [ "messages" ]

                Profile id ->
                    [ "user" ++ String.fromInt id ]

    in
    "?path=/" ++ String.join "/" pieces
