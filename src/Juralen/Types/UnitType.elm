module Juralen.Types.UnitType exposing (..)


type UnitType
    = Soldier
    | Warrior
    | Archer
    | Knight
    | Rogue
    | Wizard
    | Priest


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
