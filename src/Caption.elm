module Caption exposing (view)

import Html
import Data
import TableConfig


view : model -> List String -> TableConfig.Config msg model -> Html.Html msg
view model data config = case config.caption of
    Maybe.Nothing ->
        Html.text ""
    Maybe.Just capt ->
        Html.caption [] [ Html.text capt ]