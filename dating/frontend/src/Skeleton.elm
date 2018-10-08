module Skeleton exposing (..)

import Browser
import Html exposing (Html, h1, text, p, div)

type alias Details msg =
  { title : String
  , kids : List (Html msg) }


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
  { title =
      details.title
  , body =
      [ h1 [] [text details.title]
      , Html.map toMsg <| div [] details.kids
      , p [] [text "footer"]
      ]
  }
