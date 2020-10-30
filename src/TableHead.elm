module TableHead exposing (view)

import TableConfig
import Data
import Html
import Cell


view : model -> TableConfig.Config msg model-> List String -> Html.Html msg
view model config data =
    Html.thead []
        [ Html.tr []
            <| List.indexedMap (Cell.headerView model data) config.columns
        ]