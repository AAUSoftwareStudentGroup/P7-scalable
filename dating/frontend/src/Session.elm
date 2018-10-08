module Session exposing(..)

-- TYPES

type Data
    = LoggedIn String
    | Guest

empty : Data
empty = Guest
