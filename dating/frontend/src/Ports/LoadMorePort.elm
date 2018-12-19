port module Ports.LoadMorePort exposing (LoadMoreData, loadMore)


type alias LoadMoreData =
    Bool


port loadMore : (LoadMoreData -> msg) -> Sub msg
