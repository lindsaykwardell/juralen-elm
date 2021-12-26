module Game.NewPlayer exposing (..)

import Game.AnalyzerMode
import Game.Player exposing (NewPlayer)
import Game.PlayerColor
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


decoder : Decoder NewPlayer
decoder =
    Decode.succeed NewPlayer
        |> Decode.required "id" Decode.int
        |> Decode.required "name" Decode.string
        |> Decode.required "isHuman" Decode.bool
        |> Decode.required "analyzer" Game.AnalyzerMode.decoder
        |> Decode.required "color" Game.PlayerColor.decoder


encoder : NewPlayer -> Encode.Value
encoder newPlayer =
    Encode.object
        [ ( "id", Encode.int newPlayer.id )
        , ( "name", Encode.string newPlayer.name )
        , ( "isHuman", Encode.bool newPlayer.isHuman )
        , ( "analyzer", Game.AnalyzerMode.encoder newPlayer.analyzer )
        , ( "color", Game.PlayerColor.encoder newPlayer.color )
        ]
