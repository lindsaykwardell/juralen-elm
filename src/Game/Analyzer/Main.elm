port module Game.Analyzer.Main exposing (main)

import Game.Analysis as Analysis
import Game.Analyzer as Analyzer
import Game.Core as Core
import Json.Decode as Decode
import Json.Encode as Encode
import Platform


port analyze : (String -> msg) -> Sub msg


port analyzed : String -> Cmd msg


port logError : String -> Cmd msg


type Msg
    = Analyze String


init : () -> ( {}, Cmd msg )
init _ =
    ( {}, Cmd.none )


update : Msg -> {} -> ( {}, Cmd msg )
update (Analyze json) _ =
    case Decode.decodeString Core.decoder json of
        Ok game ->
            let
                result =
                    Analyzer.analyze game
            in
            case result of
                Nothing ->
                    ( {}, Encode.null |> Encode.encode 0 |> analyzed )

                Just option ->
                    ( {}, option |> Analysis.encoder |> Encode.encode 0 |> analyzed )

        Err err ->
            ( {}, Decode.errorToString err |> logError )


subscriptions : {} -> Sub Msg
subscriptions _ =
    analyze Analyze


main =
    Platform.worker { init = init, update = update, subscriptions = subscriptions }
