module Cell exposing (headerView, bodyView)

import Html
import Data
import ColumnConfig


headerView : model -> List String -> Int -> ColumnConfig.Column msg model -> Html.Html msg
headerView model data columnIndex column  = case column of
    ColumnConfig.Column [] ->
        Html.text ""
    ColumnConfig.Column _ ->
        let 
            columnConfig = ColumnConfig.adjustConfig column
        in
            Html.th [] <| columnConfig.titleCallback model data columnIndex column


bodyView : model -> List String -> Int -> String -> Int -> ColumnConfig.Column msg model -> Html.Html msg
bodyView model data rowIndex row columnIndex column = case column of
    ColumnConfig.Column [] ->
        Html.text ""
    ColumnConfig.Column _ ->
        let 
            columnConfig = ColumnConfig.adjustConfig column
        in
            Html.td [] 
                <| columnConfig.cellCallback model data rowIndex row columnIndex column