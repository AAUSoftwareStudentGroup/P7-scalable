module UI.Styles exposing (colorBlack, colorWhite, contentStyle, fillStyles, fonts, footerElementStyle, footerStyles, headerLogoStyles, headerNavLinkStyles, headerNavStyles, headerStyles, linkStyles, primaryColor, primaryColorD, primaryColorL, secondaryColor, secondaryColorD, secondaryColorL, siteStyles)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region


siteStyles =
    [ spacing 0
    , centerX
    , fonts
    ]


headerStyles =
    [ padding 5
    , width fill
    , Background.color primaryColor
    ]


headerLogoStyles =
    [ padding 24
    , Font.size 36
    ]


headerNavStyles =
    [ Region.navigation
    , alignRight
    , spacing 24
    , Font.size 24
    ]


headerNavLinkStyles =
    [ padding 24
    ]


footerStyles =
    [ Region.footer
    , width fill
    , padding 5
    , Font.color colorWhite
    , alignBottom
    , Background.color primaryColor
    ]


footerElementStyle =
    [ centerX
    ]


contentStyle =
    [ Region.mainContent
    , centerX
    , padding 30
    , Background.color colorWhite
    ]


linkStyles =
    [ Font.color colorWhite
    ]


fillStyles =
    [ width fill
    , height fill
    ]


fonts =
    Font.family
        [ Font.typeface "-apple-system"
        , Font.typeface "BlinkMacSystemFont"
        , Font.typeface "Segoe UI"
        , Font.typeface "Roboto"
        , Font.typeface "Oxygen-Sans"
        , Font.typeface "Ubuntu"
        , Font.typeface "Cantarell"
        , Font.typeface "Helvetica Neue"
        , Font.sansSerif
        ]


colorBlack =
    rgb 0 0 0


colorWhite =
    rgb 1 1 1


primaryColor =
    rgb255 239 83 80


primaryColorL =
    rgb255 255 255 255


primaryColorD =
    rgb255 158 167 170


secondaryColor =
    rgb255 96 125 139


secondaryColorL =
    rgb255 142 172 187


secondaryColorD =
    rgb255 52 81 94
