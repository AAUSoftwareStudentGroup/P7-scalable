port module Ports.LoadMorePort exposing (..)

type alias LoadMoreData = Bool

port loadMore : (LoadMoreData -> msg) -> Sub msg
