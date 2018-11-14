port module Page.ImgPort exposing (..)

type alias ImagePortData =
    { error    : String
    , contents : String
    , fileName : String
    }

port fileSelected : (() -> Cmd msg)

port fileContentRead : (ImagePortData -> msg) -> Sub msg
