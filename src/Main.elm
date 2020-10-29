import Browser
import Http
import Html exposing(Html, div, a, text)
import Html.Attributes exposing (href)
import Json.Decode as JD
import Json.Encode as JE
import Debug

import CustomTable as CT


main : Program () Model Msg
main = 
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type Msg
    = GotData (Result Http.Error String)
    | Log String


type alias Model = 
    { data: List String
    }


init : () -> (Model, Cmd Msg)
init _ =
    ( { data = [] }
    , Http.get
        { url = "https://randomuser.me/api/?page=1&results=30"
        , expect = Http.expectString GotData
        }
    )


-- UPDATE

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotData result ->
            case result of
                Ok jsonString ->
                    case JD.decodeString (JD.field "results" <| JD.list JD.value) jsonString of
                        Ok newData -> 
                            ({ model | data = List.map (JE.encode 0) newData }, Cmd.none)
                        
                        Err e -> 
                            (\_ -> (model, Cmd.none)) <| Debug.log "JSON ERROR!" <| JD.errorToString e

                Err e ->
                    (\_ -> (model, Cmd.none)) <| Debug.log "HTTP ERROR!" <| httpErrorToString e
        
        Log msg_ -> 
                (\_ -> (model, Cmd.none)) <| Debug.log "log" msg_


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ CT.view 
            [ CT.Caption (Maybe.Just "Test Custom Table")
            , CT.Columns 
                [ CT.Column [ CT.Title "ID",  CT.CellPath "login.uuid" ] 
                , CT.Column [ CT.CellView (\_ _ -> [ a [ href "https://ya.ru" ] [ text "Yandex !" ] ]) ]
                , CT.Column [ CT.CellPath "location.street.name", CT.CellPath "location.street.number" ]
                ]
            ] 
            model.data 
            model 
        ]        


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus _ ->
            "Unknown error"
        Http.BadBody errorMessage ->
            errorMessage