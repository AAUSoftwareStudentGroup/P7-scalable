port module Page.ImgPort exposing (..)

type alias ImagePortData =
  { contents : String
  , filename : String
  }

port fileSelected : String -> Cmd msg

port fileContentRead : (ImagePortData -> msg) -> Sub msg