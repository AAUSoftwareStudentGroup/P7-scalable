module Session exposing(..)

import Browser.Navigation as Nav

-- TYPES

type Data
    = LoggedIn Nav.Key String
    | Guest Nav.Key

empty : Nav.Key -> Data
empty key = Guest key

navKey : Data -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key