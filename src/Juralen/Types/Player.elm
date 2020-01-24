module Juralen.Types.Player exposing (..)

import Juralen.Types.Resources exposing (Resources)


type alias Player =
    { id : Int
    , name : String
    , resources : Resources
    , hasLost : Bool
    , isHuman : Bool
    , color : String
    }


type alias NewPlayer =
    { name : String
    , isHuman : Bool
    , color : String
    }


generatePlayer : NewPlayer -> Int -> Player
generatePlayer player id =
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

findPlayer : List Player -> Maybe Int -> String
findPlayer players controlledBy =
    case controlledBy of
        Nothing ->
            ""

        Just playerId ->
            case List.head (List.filter (\p -> p.id == playerId) players) of
                Nothing ->
                    ""

                Just player ->
                    player.name

findPlayerColor : List Player -> Maybe Int -> String
findPlayerColor players controlledBy =
    case controlledBy of
        Nothing ->
            ""

        Just playerId ->
            case List.head (List.filter (\p -> p.id == playerId) players) of
                Nothing ->
                    ""

                Just player ->
                    player.color
