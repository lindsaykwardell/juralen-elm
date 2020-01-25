module Juralen.Types.Player exposing (..)

import Juralen.Types.Resources exposing (Resources)


type PlayerColor
    = Red
    | Blue
    | Green
    | Orange
    | Teal
    | Purple
    | Yellow
    | Gray


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
                    case player.color of
                        Red ->
                            "player-red"

                        Blue ->
                            "player-blue"

                        Green ->
                            "player-green"

                        Orange ->
                            "player-orange"

                        Teal ->
                            "player-teal"

                        Yellow ->
                            "player-yellow"

                        Purple ->
                            "player-purple"

                        Gray ->
                            "player-gray"
