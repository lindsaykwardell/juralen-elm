module Game.UnitType exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


type UnitType
    = Soldier
    | Warrior
    | Archer
    | Knight
    | Rogue
    | Wizard
    | Priest


decoder : Decoder UnitType
decoder =
    Decode.string
        |> Decode.andThen
            (\unitType ->
                case unitType of
                    "soldier" ->
                        Decode.succeed Soldier

                    "warrior" ->
                        Decode.succeed Warrior

                    "archer" ->
                        Decode.succeed Archer

                    "knight" ->
                        Decode.succeed Knight

                    "rogue" ->
                        Decode.succeed Rogue

                    "wizard" ->
                        Decode.succeed Wizard

                    "priest" ->
                        Decode.succeed Priest

                    _ ->
                        Decode.fail "Invalid unit type"
            )


encoder : UnitType -> Encode.Value
encoder unitType =
    case unitType of
        Soldier ->
            Encode.string "soldier"

        Warrior ->
            Encode.string "warrior"

        Archer ->
            Encode.string "archer"

        Knight ->
            Encode.string "knight"

        Rogue ->
            Encode.string "rogue"

        Wizard ->
            Encode.string "wizard"

        Priest ->
            Encode.string "priest"


type alias InitialValues =
    { movesLeft : Int
    , attack : Int
    , health : Int
    , range : Int
    }


cost : UnitType -> Int
cost unitType =
    case unitType of
        Soldier ->
            1

        Warrior ->
            2

        Archer ->
            2

        Knight ->
            4

        Rogue ->
            4

        Wizard ->
            6

        Priest ->
            6


moveCost : UnitType -> Float
moveCost unitType =
    case unitType of
        -- Wizard ->
        --     0.25
        _ ->
            1


threat : UnitType -> Int
threat unitType =
    case unitType of
        Soldier ->
            1

        Warrior ->
            2

        Archer ->
            2

        Knight ->
            3

        Rogue ->
            3

        Wizard ->
            4

        Priest ->
            4


toString : UnitType -> String
toString unitType =
    case unitType of
        Soldier ->
            "Soldier"

        Warrior ->
            "Warrior"

        Archer ->
            "Archer"

        Knight ->
            "Knight"

        Rogue ->
            "Rogue"

        Wizard ->
            "Wizard"

        Priest ->
            "Priest"


short : UnitType -> String
short unitType =
    case unitType of
        Soldier ->
            "So"

        Warrior ->
            "Wa"

        Archer ->
            "Ar"

        Knight ->
            "Kn"

        Rogue ->
            "Ro"

        Wizard ->
            "Wi"

        Priest ->
            "Pr"


icon : UnitType -> String
icon unitType =
    case unitType of
        Soldier ->
            "/img/soldier.png"

        Warrior ->
            "/img/warrior.png"

        Archer ->
            "/img/archer.jpg"

        Knight ->
            "/img/knight.png"

        Rogue ->
            "/img/rogue.jpg"

        Wizard ->
            "/img/wizard.png"

        Priest ->
            "/img/priest.jpg"


initialValues : UnitType -> InitialValues
initialValues unitType =
    case unitType of
        Soldier ->
            { movesLeft = 1
            , attack = 1
            , health = 2
            , range = 1
            }

        Warrior ->
            { movesLeft = 1
            , attack = 2
            , health = 2
            , range = 1
            }

        Archer ->
            { movesLeft = 1
            , attack = 1
            , health = 3
            , range = 2
            }

        Knight ->
            { movesLeft = 2
            , attack = 2
            , health = 3
            , range = 1
            }

        Rogue ->
            { movesLeft = 2
            , attack = 3
            , health = 2
            , range = 1
            }

        Wizard ->
            { movesLeft = 3
            , attack = 3
            , health = 3
            , range = 2
            }

        Priest ->
            { movesLeft = 1
            , attack = 1
            , health = 5
            , range = 1
            }
