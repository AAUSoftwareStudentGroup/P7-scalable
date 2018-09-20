module Main exposing (Model, Msg(..), Styles(..), init, main, sansSerif, stylesheet, subscriptions, update, view)

import Color
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events
import Element.Input as Input
import Html
import Style exposing (..)
import Style.Background as Background
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Shadow as Shadow
import Style.Transition as Transition



-- MAIN


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "", Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


type Styles
    = None
    | Main
    | Page
    | Logo
    | NavOption
    | Box
    | Container
    | Label
    | Field
    | Button


sansSerif =
    [ Font.font "helvetica"
    , Font.font "arial"
    , Font.font "sans-serif"
    ]


stylesheet =
    styleSheet
        [ style None []
        , style Main
            [ Border.all 1 -- set all border widths to 1 px.
            , Color.text Color.darkCharcoal
            , Color.background Color.white
            , Color.border Color.lightGrey
            , Font.typeface sansSerif
            , Font.size 16
            , Font.lineHeight 1.3 -- line height, given as a ratio of current font size.
            ]
        , style Page
            [ Border.all 5
            , Border.solid
            , Color.text Color.darkCharcoal
            , Color.background Color.white
            , Color.border Color.lightGrey
            ]
        , style Label
            [ Font.size 25 -- set font size to 25 px
            , Font.center
            ]
        , style Logo
            [ Font.size 25
            , Font.typeface sansSerif
            ]
        , style NavOption
            [ Font.size 16
            , Font.typeface sansSerif
            ]
        , style Box
            [ Transition.all
            , Color.text Color.white
            , Color.background Color.blue
            , Color.border Color.blue
            , Border.rounded 3 -- round all borders to 3px
            , hover
                [ Color.text Color.white
                , Color.background Color.red
                , Color.border Color.red
                , cursor "pointer"
                ]
            ]
        , style Container
            [ Color.text Color.black
            , Color.background Color.lightGrey
            , Color.border Color.lightGrey
            ]
        , style Field
            [ Border.rounded 5
            , Border.all 1
            , Border.solid
            , Color.border Color.lightGrey
            ]
        , style Button
            [ Border.rounded 5
            , Border.all 1
            , Border.solid
            , Color.border Color.blue
            , Color.background Color.lightBlue
            ]
        ]


view : Model -> Html.Html Msg
view model =
    layout stylesheet <|
        column None
            []
            [ navigation
            , el None [ center, width (px 800) ] <|
                column Main
                    [ spacing 50, paddingTop 50, paddingBottom 50 ]
                    (List.concat [ viewTextLayout, viewRowLayouts, viewGridLayout ])
            ]


navigation =
    row None
        [ spread, paddingXY 80 20 ]
        [ el Logo [] (text "Dating")
        , row None
            [ spacing 20, alignBottom ]
            [ el NavOption [] (text "user list")
            , el NavOption [] (text "about")
            , el NavOption [] (text "user profile")
            ]
        ]

viewTextLayout =
    [ el Label [] (text "First, Some Text")
    , textLayout None
        [ spacingXY 25 25
        , padding 60
        ]
        [ el Box
            [ width (px 200)
            , height (px 300)
            , alignLeft
            ]
            (text "Alignment attributes (such as alignLeft), work in all layouts. In a text layout, the result is the element is floated left.")
        , paragraph None
            []
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris vel lectus eget lorem lobortis suscipit. Fusce porta auctor purus sed tempor. Mauris auctor sapien sit amet elementum egestas. Maecenas placerat consequat mauris, at dapibus enim tristique a. Quisque feugiat ultricies lorem nec volutpat. Sed risus enim, facilisis id fermentum quis, eleifend in diam. Suspendisse euismod, urna nec consectetur volutpat, massa libero aliquam urna, hendrerit venenatis leo lacus faucibus nulla. Curabitur et mattis dolor."
            ]
        , hairline Container
        , paragraph None
            []
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris vel lectus eget lorem lobortis suscipit. Fusce porta auctor purus sed tempor. Mauris auctor sapien sit amet elementum egestas. Maecenas placerat consequat mauris, at dapibus enim tristique a. Quisque feugiat ultricies lorem nec volutpat. Sed risus enim, facilisis id fermentum quis, eleifend in diam. Suspendisse euismod, urna nec consectetur volutpat, massa libero aliquam urna, hendrerit venenatis leo lacus faucibus nulla. Curabitur et mattis dolor."
            ]
        , paragraph None
            [ width (px 500)
            , center
            ]
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris vel lectus eget lorem lobortis suscipit. Fusce porta auctor purus sed tempor. Mauris auctor sapien sit amet elementum egestas. Maecenas placerat consequat mauris, at dapibus enim tristique a. Quisque feugiat ultricies lorem nec volutpat. Sed risus enim, facilisis id fermentum quis, eleifend in diam. Suspendisse euismod, urna nec consectetur volutpat, massa libero aliquam urna, hendrerit venenatis leo lacus faucibus nulla. Curabitur et mattis dolor."
            ]
        , paragraph None
            []
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris vel lectus eget lorem lobortis suscipit. Fusce porta auctor purus sed tempor. Mauris auctor sapien sit amet elementum egestas. Maecenas placerat consequat mauris, at dapibus enim tristique a. Quisque feugiat ultricies lorem nec volutpat. Sed risus enim, facilisis id fermentum quis, eleifend in diam. Suspendisse euismod, urna nec consectetur volutpat, massa libero aliquam urna, hendrerit venenatis leo lacus faucibus nulla. Curabitur et mattis dolor."
            ]
        , el Box
            [ width (px 200)
            , height (px 300)
            , alignRight
            , spacing 100
            ]
            empty
        , paragraph None
            []
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris vel lectus eget lorem lobortis suscipit. Fusce porta auctor purus sed tempor. Mauris auctor sapien sit amet elementum egestas. Maecenas placerat consequat mauris, at dapibus enim tristique a. Quisque feugiat ultricies lorem nec volutpat. Sed risus enim, facilisis id fermentum quis, eleifend in diam. Suspendisse euismod, urna nec consectetur volutpat, massa libero aliquam urna, hendrerit venenatis leo lacus faucibus nulla. Curabitur et mattis dolor."
            ]
        , paragraph None
            []
            [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris vel lectus eget lorem lobortis suscipit. Fusce porta auctor purus sed tempor. Mauris auctor sapien sit amet elementum egestas. Maecenas placerat consequat mauris, at dapibus enim tristique a. Quisque feugiat ultricies lorem nec volutpat. Sed risus enim, facilisis id fermentum quis, eleifend in diam. Suspendisse euismod, urna nec consectetur volutpat, massa libero aliquam urna, hendrerit venenatis leo lacus faucibus nulla. Curabitur et mattis dolor."
            ]

        -- a "full" will expand to consume the parent padding.
        , full Box [] <|
            text "A Full element will grow to consume parent padding!"
        ]
    ]


viewRowLayouts =
    [ el Label [] (text "Here is a Row Layout")
    , row Container
        [ spacingXY 20 20 ]
        [ el Box [ width (px 100), height (px 100) ] empty
        , el Box [ width (px 100), height (px 100) ] empty
        , el Box [ width (px 100), height (px 100) ] empty
        ]
    , el Label [] (text "You can Align Children Individually")
    , row Container
        [ spacingXY 20 20, height (px 400) ]
        [ el Box [ width (px 100), height (px 100), alignTop ] (text "top")
        , el Box [ width (px 100), height (px 100), verticalCenter ] (text "vcenter")
        , el Box [ width (px 100), height (px 100), alignBottom ] (text "bottom")
        ]
    , el Label [] (text "Or you can set the alignment for an entire layout.")
    , row Container
        [ spacingXY 20 20, alignRight ]
        [ el Box [ width (px 100), height (px 100) ] empty
        , el Box [ width (px 100), height (px 100) ] empty
        , el Box [ width (px 100), height (px 100) ] empty
        ]
    ]


viewGridLayout =
    [ el Label [] (text "Grid Layout")
    , grid Container
        [ spacing 20 ]
        { columns = [ px 100, px 100, px 100, px 100 ]
        , rows =
            [ px 100
            , px 100
            , px 100
            , px 100
            ]
        , cells =
            [ cell
                { start = ( 0, 0 )
                , width = 1
                , height = 1
                , content = (el Box [] (text "box"))
                }
            , cell
                { start = ( 1, 1 )
                , width = 1
                , height = 2
                , content = (el Box [ spacing 100 ] (text "box"))
                }
            , cell
                { start = ( 2, 1 )
                , width = 2
                , height = 2
                , content = (el Box [] (text "box"))
                }
            , cell
                { start = ( 1, 0 )
                , width = 1
                , height = 1
                , content = (el Box [] (text "box"))
                }
            ]
        }
    ]
