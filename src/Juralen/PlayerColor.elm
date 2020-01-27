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
    | None


toString : PlayerColor -> String
toString playerColor =
    case playerColor of
        Red ->
            "red"

        Blue ->
            "blue"

        Green ->
            "green"

        Orange ->
            "orange"

        Teal ->
            "teal"

        Yellow ->
            "yellow"

        Purple ->
            "purple"

        Gray ->
            "gray"

        None ->
            ""

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

        None ->
            ""