module Session exposing(..)

import Browser.Navigation as Nav

-- TYPES

type Session
    = LoggedIn Nav.Key String
    | Guest Nav.Key

empty : Nav.Key -> Session
empty key = Guest key

navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key
