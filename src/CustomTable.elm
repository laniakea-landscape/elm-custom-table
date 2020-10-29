module CustomTable exposing 
    ( view, Property(..)
    , Column(..), ColProperty(..)
    )

import Html exposing 
    ( Html, text, table
    , thead, tbody, tr
    , th, td, tfoot
    , caption
    )
import Json.Decode as JD
import Json.Encode as JE
import Html exposing (col)


type Property msg model
    = Columns (List (Column msg model))
    | Caption (Maybe String)
    | NoHeaderRow


type alias Config msg model = 
    { columns: List (Column msg model)
    , caption: Maybe String
    , noHeaderRow: Bool
    }


defaultConfig : Config msg model
defaultConfig =
    { columns = []
    , caption = Maybe.Nothing
    , noHeaderRow = False
    }


adjustConfig : List (Property msg model) -> Config msg model -> Config msg model 
adjustConfig props acc =
    case props of
        [] -> 
            acc
        p :: ps -> 
            adjustConfig ps
                <| case p of
                        Columns cols ->
                            { acc | columns = cols }
                        Caption cptn ->
                            { acc | caption = cptn }
                        NoHeaderRow -> 
                            { acc | noHeaderRow = True }


type alias Row = String


type alias Data = List Row


view : List (Property msg model) -> List String -> model -> Html msg 
view props data model =
    let config = adjustConfig props defaultConfig
    in
    table []
        [ thead [] 
            [ viewCaption config 
            , viewHead model config
            , viewBody model config data
            ]
        ]


viewCaption : Config msg model -> Html msg
viewCaption config = case config.caption of
    Maybe.Nothing ->
        text ""
    Maybe.Just capt ->
        caption [] [ text capt ]


viewHead : model -> Config msg model -> Html msg
viewHead model config = case config.noHeaderRow of
    True ->
        text ""
    False ->
        thead []
            [ tr []
                <| List.indexedMap (viewHeadColumn model) config.columns
            ]


-- COLUMNS

type ColProperty msg model
    = Title String
    | TitleView (model -> List (Html msg))
    | CellPath String
    | CellView (String -> model -> List (Html msg))


type Column msg model = Column (List (ColProperty msg model))


type alias ColConfig msg model =
    { titleCallback : (model -> List (Html msg))
    , cellCallback : (String -> model -> List (Html msg))
    }


defaultColConfig : Int -> ColConfig msg model 
defaultColConfig colIndex = 
    { titleCallback = (\_ -> [text ("Column #" ++ String.fromInt colIndex)])
    , cellCallback = (\_ _ -> [text "No data"])
    }


adjustColConfig : Column msg model -> ColConfig msg model -> ColConfig msg model 
adjustColConfig column acc =
    case column of
        Column [] -> 
            acc
        Column (p :: ps) -> 
            adjustColConfig (Column ps)
                <| case p of
                    Title title ->
                        { acc | titleCallback = (\_ -> [text title]) }
                    TitleView titleView ->
                        { acc | titleCallback = titleView }
                    CellPath path -> 
                        { acc | cellCallback = (\row _ -> [text <| pathDecoder row path]) }
                    CellView cellView ->
                        { acc | cellCallback = cellView }


pathDecoder : String -> String -> String
pathDecoder json path =
    case JD.decodeString (JD.at (String.split "." path) (JD.oneOf [ JD.string, everyDecoder ])) json of
        Ok data -> data
        Err e -> Debug.log "ERROR!" <| JD.errorToString e


everyDecoder : JD.Decoder String
everyDecoder = JD.oneOf 
    [ JD.string
    , JD.int |> JD.andThen (\i -> JD.succeed <| String.fromInt i)
    , JD.float |> JD.andThen (\f -> JD.succeed <| String.fromFloat f)
    ]


viewHeadColumn : model -> Int -> Column msg model -> Html msg
viewHeadColumn model index column  = case column of
    Column [] ->
        text ""
    Column props ->
        let colConfig : ColConfig msg model
            colConfig = adjustColConfig column (defaultColConfig index)
        in
            th [] <| colConfig.titleCallback model

        
viewBody : model -> Config msg model -> List String -> Html msg
viewBody model config data =
    tbody []
        <| List.map (viewRow model config) data


viewRow : model -> Config msg model -> String -> Html msg
viewRow model config row =
    tr []
        <| List.indexedMap (viewBodyColumn model row) config.columns


viewBodyColumn : model -> String -> Int -> Column msg model -> Html msg
viewBodyColumn model row colIndex column = case column of
    Column [] ->
        text ""
    Column props ->
        let colConfig : ColConfig msg model
            colConfig = adjustColConfig column (defaultColConfig colIndex)
        in
            td [] 
                <| colConfig.cellCallback row model
            