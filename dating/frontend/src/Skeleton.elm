module Skeleton exposing (Details, view)

import Browser
import Element exposing (Element)
import Html exposing (Html)
import Routing exposing (Route(..))
import Session exposing (Session)
import UI.Elements as El
import UI.Styles exposing (..)


type alias Details msg =
    { title : String
    , session : Session
    , kids : List (Element msg)
    }


view : (a -> msg) -> Details a -> Browser.Document msg
view toMsg details =
    { title = details.title
    , body = El.site toMsg details.kids details.session
    }
