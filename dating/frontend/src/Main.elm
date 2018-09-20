module Main exposing (..)

{-| -}

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region


white =
    Element.rgb 1 1 1


grey =
    Element.rgb 0.9 0.9 0.9


blue =
    Element.rgb 0 0 0.8


red =
    Element.rgb 0.8 0 0


darkBlue =
    Element.rgb 0 0 0.9


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init =
    { username = ""
    , password = ""
    , passwordAgain = ""
    , profileText = ""
    , gender = Male
    }


type alias Form =
    { username : String
    , password : String
    , passwordAgain : String
    , profileText : String
    , gender : Gender
    }


type Msg
    = Update Form


update msg model =
    case Debug.log "msg" msg of
        Update new ->
            new


type Gender
    = Male
    | Female
    | Other


view model =
    Element.layout
        [ Font.size 20
        ]
    <|
        Element.column [ width (px 800), height shrink, centerY, centerX, spacing 36, padding 10] -- , explain Debug.todo ]
            [ el
                [ Region.heading 1
                , centerX
                , Font.size 36
                ]
                (text "User creation")
            , Input.username
                [ spacing 12
                , below (showWarningIfUsernameIsTaken model)
                ]
                { text = model.username
                , placeholder = Just (Input.placeholder [] (text "username"))
                , onChange = \new -> Update { model | username = new }
                , label = Input.labelAbove [ Font.size 14 ] (text "Username")
                }
            , Input.newPassword [ spacing 12, width shrink ]
                { text = model.password
                , placeholder = Nothing
                , onChange = \new -> Update { model | password = new }
                , label = Input.labelAbove [ Font.size 14 ] (text "Password")
                , show = False
                }
            , Input.newPassword [ spacing 12, width shrink, below (maybeShowPasswordsNotEqualWarning model) ]
                { text = model.passwordAgain
                , placeholder = Nothing
                , onChange = \new -> Update { model | passwordAgain = new }
                , label = Input.labelAbove [ Font.size 14 ] (text "Repeat password")
                , show = False
                }
            , Input.radio
                [ spacing 12
                ]
                { selected = Just model.gender
                , onChange = \new -> Update { model | gender = new }
                , label = Input.labelAbove [ Font.size 14, paddingXY 0 12 ] (text "Gender")
                , options =
                    [ Input.option Male (text "Man")
                    , Input.option Female (text "Woman")
                    , Input.option Other (text "Other")
                    ]
                }
            , Input.multiline
                [ height shrink
                , spacing 12

                -- , padding 6
                ]
                { text = model.profileText
                , placeholder = Just (Input.placeholder [] (text "I like big butts and I cannot lie."))
                , onChange = \new -> Update { model | profileText = new }
                , label = Input.labelAbove [ Font.size 14 ] (text "Describe yourself")
                , spellcheck = False
                }
            , Input.button
                [ Background.color red
                , Font.color white
                , Border.color darkBlue
                , paddingXY 32 16
                , Border.rounded 3
                , width fill
                ]
                { onPress = Nothing
                , label = Element.text "Create!"
                }
            ]

showWarningIfUsernameIsTaken model = if model.username == "Bargsteen"
                             then mkWarning "Username is taken"
                             else none

mkWarning warning = el
                        [ Font.color red
                        , Font.size 14
                        , alignRight
                        , moveDown 6
                        ]
                        (text warning)


maybeShowPasswordsNotEqualWarning model = if model.passwordAgain /= "" && model.password /= model.passwordAgain
                                          then mkWarning "Passwords do not match"
                                          else none
