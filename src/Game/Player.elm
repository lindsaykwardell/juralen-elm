module Game.Player exposing (..)

import Game.AnalyzerMode exposing (AnalyzerMode)
import Game.PlayerColor exposing (PlayerColor)
import Game.Resources exposing (Resources)
import Game.TechTree as TechTree exposing (TechTree)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import List.Extra as List


type alias Player =
    { id : Int
    , name : String
    , resources : Resources
    , isHuman : Bool
    , analyzer : AnalyzerMode
    , color : PlayerColor
    , techTree : TechTree
    }


decoder : Decoder Player
decoder =
    Decode.succeed Player
        |> Decode.required "id" Decode.int
        |> Decode.required "name" Decode.string
        |> Decode.required "resources" Game.Resources.decoder
        |> Decode.required "isHuman" Decode.bool
        |> Decode.required "analyzer" Game.AnalyzerMode.decoder
        |> Decode.required "color" Game.PlayerColor.decoder
        |> Decode.required "techTree" TechTree.decoder


encoder : Player -> Encode.Value
encoder player =
    Encode.object
        [ ( "id", Encode.int player.id )
        , ( "name", Encode.string player.name )
        , ( "resources", Game.Resources.encoder player.resources )
        , ( "isHuman", Encode.bool player.isHuman )
        , ( "analyzer", Game.AnalyzerMode.encoder player.analyzer )
        , ( "color", Game.PlayerColor.encoder player.color )
        , ( "techTree", TechTree.encoder player.techTree )
        ]


type alias NewPlayer =
    { id : Int
    , name : String
    , isHuman : Bool
    , analyzer : AnalyzerMode
    , color : PlayerColor
    }


generate : NewPlayer -> Int -> Player
generate player id =
    { id = id
    , name = player.name
    , resources =
        { actions = 1
        , gold = 2
        }
    , isHuman = player.isHuman
    , analyzer = player.analyzer
    , color = player.color
    , techTree = TechTree.empty
    }


revertToNewPlayer : Player -> NewPlayer
revertToNewPlayer player =
    { id = player.id
    , name = player.name
    , isHuman = player.isHuman
    , analyzer = player.analyzer
    , color = player.color
    }


empty : Player
empty =
    { id = -1
    , name = ""
    , resources =
        { actions = -1
        , gold = -1
        }
    , isHuman = False
    , analyzer = Game.AnalyzerMode.Default
    , color = Game.PlayerColor.Gray
    , techTree = TechTree.empty
    }


get : List Player -> Int -> Player
get players playerId =
    case List.find (\p -> p.id == playerId) players of
        Nothing ->
            empty

        Just player ->
            player
