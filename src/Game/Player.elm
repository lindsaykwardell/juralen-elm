module Game.Player exposing (..)

import Game.AnalyzerMode exposing (AnalyzerMode)
import Game.PlayerColor exposing (PlayerColor)
import Game.Resources exposing (Resources)
import Game.TechTree as TechTree exposing (TechTree)
import List.Extra as List


type alias Player =
    { id : Int
    , name : String
    , resources : Resources
    , hasLost : Bool
    , isHuman : Bool
    , analyzer : AnalyzerMode
    , color : PlayerColor
    , score : Int
    , techTree : TechTree
    }


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
    , hasLost = False
    , isHuman = player.isHuman
    , analyzer = player.analyzer
    , color = player.color
    , score = 2
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
    , hasLost = True
    , isHuman = False
    , analyzer = Game.AnalyzerMode.Default
    , color = Game.PlayerColor.Gray
    , score = 0
    , techTree = TechTree.empty
    }


get : List Player -> Int -> Player
get players playerId =
    case List.find (\p -> p.id == playerId) players of
        Nothing ->
            empty

        Just player ->
            player
