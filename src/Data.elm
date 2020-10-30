module Data exposing (pathDecoder)

import Json.Decode as JD


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