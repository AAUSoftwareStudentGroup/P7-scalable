module Page.Header exposing (getHeader)

import Browser
import Browser.Dom as Dom
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import GenHelpers exposing (Gender(..))
import Generated.DatingApi exposing (..)
import Http
import String


getHeader : Maybe User -> Element msg
getHeader _ =
    Element.row headerStyle
        [ el
            [ Region.heading 1 ]
            (text "Welcome")
        ]


headerStyle =
    [ width fill
    , padding 5
    , Font.size 60
    , alignLeft
    , Background.color gray
    , Border.width 1
    ]


black =
    Element.rgb 0 0 0


gray =
    Element.rgb 0.5 0.5 0.5
