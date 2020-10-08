module Juralen.Player exposing (..)

import Juralen.PlayerColor exposing (PlayerColor)
import Juralen.Resources exposing (Resources)
import Juralen.AnalyzerMode exposing (AnalyzerMode)


type alias Player =
    { id : Int
    , name : String
    , resources : Resources
    , hasLost : Bool
    , isHuman : Bool
    , analyzer : AnalyzerMode
    , color : PlayerColor
    , score: Int
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
    , analyzer = Juralen.AnalyzerMode.Default
    , color = Juralen.PlayerColor.Gray
    , score = 0
    }


get : List Player -> Int -> Player
get players playerId =
    case List.head (List.filter (\p -> p.id == playerId) players) of
        Nothing ->
            empty

        Just player ->
            player


getName : List Player -> Maybe Int -> String
getName players controlledBy =
    case controlledBy of
        Nothing ->
            ""

        Just playerId ->
            case List.head (List.filter (\p -> p.id == playerId) players) of
                Nothing ->
                    ""

                Just player ->
                    player.name


getResources : List Player -> Int -> Resources
getResources players id =
    case List.head (List.filter (\p -> p.id == id) players) of
        Nothing ->
            Juralen.Resources.empty

        Just player ->
            player.resources


getColor : List Player -> Int -> PlayerColor
getColor players controlledBy =
    case List.head (List.filter (\p -> p.id == controlledBy) players) of
        Nothing ->
            Juralen.PlayerColor.None

        Just player ->
            player.color


getColorClass : List Player -> Maybe Int -> String
getColorClass players controlledBy =
    case controlledBy of
        Nothing ->
            ""

        Just playerId ->
            case List.head (List.filter (\p -> p.id == playerId) players) of
                Nothing ->
                    ""

                Just player ->
                    Juralen.PlayerColor.toClass player.color