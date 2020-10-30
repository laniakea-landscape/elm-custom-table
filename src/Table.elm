module Table exposing (view)

import Html

import Caption
import TableHead
import TableBody
import TableConfig


view : List (TableConfig.Property msg model) -> List String -> model -> Html.Html msg 
view props data model =
    let config = TableConfig.adjustConfig props 
    in
    Html.table []
        [ Caption.view model data config 
        , TableHead.view model config data
        , TableBody.view model config data
        ]
        