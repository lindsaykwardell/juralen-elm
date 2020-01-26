module Juralen.PlayerColor exposing (..)


type PlayerColor
    = Red
    | Blue
    | Green
    | Orange
    | Teal
    | Purple
    | Yellow
    | Gray


toClass : PlayerColor -> String
toClass playerColor =
    case playerColor of
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
