module TableBody exposing (view)

import Html
import Data
import TableConfig
import Cell


view : model -> TableConfig.Config msg model -> List String -> Html.Html msg
view model config data =
    Html.tbody []
        <| List.indexedMap (rowView model config data) data


rowView : model -> TableConfig.Config msg model -> List String -> Int -> String -> Html.Html msg
rowView model config data rowIndex row = 
    Html.tr []
        <| List.indexedMap (Cell.bodyView model data rowIndex row) config.columns