port module Ports.FileUploadPort exposing (FilePortData, fileContentRead, fileSelected)


type alias FilePortData =
    { error : String
    , contents : String
    , fileName : String
    }


port fileSelected : () -> Cmd msg


port fileContentRead : (FilePortData -> msg) -> Sub msg
