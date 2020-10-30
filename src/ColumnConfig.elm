module ColumnConfig exposing (Property(..), Column(..), adjustConfig)

import Html
import Data

type Property msg model
    = Title String
    | TitleView (model -> List String -> Int -> Column msg model -> List (Html.Html msg))
    | CellPath String
    | CellView (model -> List String -> Int -> String -> Int -> Column msg model -> List (Html.Html msg))


type Column msg model = Column (List (Property msg model))


type alias Config msg model =
    { titleCallback : (model -> List String -> Int -> Column msg model -> List (Html.Html msg))
    , cellCallback : (model -> List String -> Int -> String -> Int -> Column msg model -> List (Html.Html msg))
    }


defaultConfig : Config msg model 
defaultConfig =
    { titleCallback = (\model data columnIndex column -> [ Html.text ("Column #" ++ String.fromInt columnIndex) ])
    , cellCallback = (\model data rowIndex row columnIndex column -> [ Html.text "Example data" ])
    }


adjustConfig : Column msg model -> Config msg model
adjustConfig = adjustConfig_ defaultConfig 


adjustConfig_ : Config msg model -> Column msg model -> Config msg model 
adjustConfig_ acc column_ =
    case column_ of
        Column [] -> 
            acc
        Column (p :: ps) -> 
            adjustConfig_ (adjustProperty acc p) (Column ps)

adjustProperty : Config msg model -> Property msg model -> Config msg model
adjustProperty config property = case property of
    
    Title title ->
        { config | titleCallback = (\model data columnIndex column -> [ Html.text title]) }
    
    TitleView titleView ->
        { config | titleCallback = titleView }
    
    CellPath path -> 
        { config | cellCallback = (\model data rowIndex row columnIndex column -> [ Html.text <| Data.pathDecoder row path]) }
    
    CellView cellView ->
        { config | cellCallback = cellView }