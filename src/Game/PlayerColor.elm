module Game.PlayerColor exposing (..)


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


toList : List PlayerColor
toList =
    [ Red, Blue, Green, Orange, Teal, Purple, Yellow, Gray ]


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


fromString : String -> PlayerColor
fromString color =
    case color of
        "red" ->
            Red

        "blue" ->
            Blue

        "green" ->
            Green

        "orange" ->
            Orange

        "teal" ->
            Teal

        "yellow" ->
            Yellow

        "purple" ->
            Purple

        "gray" ->
            Gray

        _ ->
            None


toClass : PlayerColor -> String
toClass playerColor =
    case playerColor of
        Red ->
            "bg-player-red"

        Blue ->
            "bg-player-blue"

        Green ->
            "bg-player-green"

        Orange ->
            "bg-player-orange"

        Teal ->
            "bg-player-teal"

        Yellow ->
            "bg-player-yellow"

        Purple ->
            "bg-player-purple"

        Gray ->
            "bg-player-gray"

        None ->
            ""


isDark : PlayerColor -> Bool
isDark playerColor =
    case playerColor of
        Red ->
            True

        Blue ->
            True

        Green ->
            False

        Orange ->
            True

        Teal ->
            False

        Yellow ->
            False

        Purple ->
            True

        Gray ->
            True

        None ->
            False
