module Juralen.UnitType exposing (..)


type UnitType
    = Soldier
    | Warrior
    | Archer
    | Knight
    | Rogue
    | Wizard
    | Priest


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
            3

        Knight ->
            6

        Rogue ->
            5

        Wizard ->
            7

        Priest ->
            4

moveCost : UnitType -> Float
moveCost unitType =
    case unitType of
        Wizard ->
            0.5

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
            3

        Knight ->
            4

        Rogue ->
            3

        Wizard ->
            4

        Priest ->
            0


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
            { movesLeft = 3
            , attack = 2
            , health = 4
            , range = 1
            }

        Rogue ->
            { movesLeft = 2
            , attack = 3
            , health = 1
            , range = 1
            }

        Wizard ->
            { movesLeft = 2
            , attack = 2
            , health = 2
            , range = 2
            }

        Priest ->
            { movesLeft = 1
            , attack = 0
            , health = 5
            , range = 0
            }
