module TableConfig exposing
    ( Property(..), adjustConfig, Config)

import ColumnConfig


type Property msg model
    = Columns (List (ColumnConfig.Column msg model))
    | Caption (Maybe String)
    | NoHeaderRow


type alias Config msg model = 
    { columns: List (ColumnConfig.Column msg model)
    , caption: Maybe String
    , noHeaderRow: Bool
    }


defaultConfig : Config msg model
defaultConfig =
    { columns = []
    , caption = Maybe.Nothing
    , noHeaderRow = False
    }


adjustConfig : List (Property msg model) -> Config msg model
adjustConfig = adjustConfig_ defaultConfig


adjustConfig_ : Config msg model -> List (Property msg model) -> Config msg model 
adjustConfig_ acc props =
    case props of
        [] -> 
            acc
        p :: ps -> 
            adjustConfig_ (adjustProperty acc p) ps
                        
                        
adjustProperty : Config msg model -> Property msg model -> Config msg model
adjustProperty config property = case property of
    
    Columns cols ->
        { config | columns = cols }

    Caption cptn ->
        { config | caption = cptn }

    NoHeaderRow -> 
        { config | noHeaderRow = True }