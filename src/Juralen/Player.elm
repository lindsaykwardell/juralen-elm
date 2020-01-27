module Juralen.Player exposing (..)

import Juralen.Resources exposing (Resources)
import Juralen.PlayerColor exposing (PlayerColor)


type alias Player =
    { id : Int
    , name : String
    , resources : Resources
    , hasLost : Bool
    , isHuman : Bool
    , color : PlayerColor
    }


type alias NewPlayer =
    { name : String
    , isHuman : Bool
    , color : PlayerColor
    }

generate : NewPlayer -> Int -> Player
generate player id =
    { id = id
    , name = player.name
    , resources =
        { actions = 4
        , gold = 2
        }
    , hasLost = False
    , isHuman = player.isHuman
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
    , color = Juralen.PlayerColor.Gray
    }

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