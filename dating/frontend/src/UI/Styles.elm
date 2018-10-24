module UI.Styles exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Region as Region


siteStyle =
    [ spacing 0
    , centerX
    , fonts
    ]


headerStyle =
    [ padding 5
    , width fill
    , Background.color primaryColor
    ]


headerLogoStyle =
    [ padding 24
    , Font.size 36
    ]


headerNavStyle =
    [ Region.navigation
    , alignRight
    , spacing 24
    , Font.size 24
    ]


headerNavLinkStyle =
    [ padding 24
    ]


footerStyle =
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


mainContentStyle =
    [ Region.mainContent
    , spacingXY 0 48
    , paddingXY 0 64
    , width (px 600)
    , centerX
    , Background.color colorWhite
    , height shrink
    ]


contentHeadingStyle =
    [ Region.heading 1
    , centerX
    , Font.size 36
    , Border.color colorBlack
    ]

contentColumnStyle spacingSize =
    [ width fill
    , spacing spacingSize
    , padding 10
    ]

formInputStyle : Element msg -> List (Attribute msg)
formInputStyle belowElement =
    [ Element.below belowElement
    , spacing 12
    ]

formLabelStyle =
    [ Font.size 14
    ]

propertyStyle =
    [ spacing 12
    ]


propertyLabelStyle =
    []


propertyTextStyle =
    []

centeredFillStyle =
    [ width fill
    , height fill
    , centerY
    , centerX
    ]

buttonStyle =
    [ paddingXY 35 15
    , Border.rounded 4
    , Border.width 1
    , Border.solid
    , fonts
    , Font.size 14
    , Font.semiBold
    , Font.color primaryColor
    , mouseOver [ Font.color secondaryColorD ]
    ]


acceptButtonStyle =
    [ Background.color acceptColor] ++ buttonStyle

cancelButtonStyle =
    [ Background.color cancelColor] ++ buttonStyle

warningStyle =
    [ Font.color cancelColor
    , Font.size 14
    , alignRight
    , moveDown 6
    ]

linkStyle =
    [ Font.color colorWhite
    ]


fillStyle =
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

right =
    alignRight

left =
    alignLeft


colorBlack =
    rgb 0 0 0


colorWhite =
    rgb 1 1 1


primaryColor =
    rgb255 239 83 80


acceptColor =
    rgb255 76 175 80


cancelColor =
    rgb255 211 60 47


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

cardStyle =
    [mouseOver cardShadowHover
    , padding 16
    ] ++ cardShadow ++ fillStyle

cardShadow =
    [ Border.shadow cardShadowA
    , Border.shadow cardShadowB
    , Border.shadow cardShadowC
    ]


cardShadowA =
    { offset = ( 0.0, 2.0 )
    , size = 0
    , blur = 2.0
    , color = rgba 0 0 0 0.14
    }


cardShadowB =
    { offset = ( 0.0, 3.0 )
    , size = -2
    , blur = 1.0
    , color = rgba 0 0 0 0.12
    }


cardShadowC =
    { offset = ( 0.0, 1.0 )
    , size = 0
    , blur = 5.0
    , color = rgba 0 0 0 0.2
    }


cardShadowHover =
    [ Border.shadow cardShadowHoverA
    , Border.shadow cardShadowHoverB
    ]


cardShadowHoverA =
    { offset = ( 0.0, 8.0 )
    , size = 0
    , blur = 17
    , color = rgba 0 0 0 0.2
    }


cardShadowHoverB =
    { offset = ( 0.0, 6.0 )
    , size = 0
    , blur = 20
    , color = rgba 0 0 0 0.19
    }

