module Juralen.UnitType exposing (..)

import Juralen.TechTree exposing (TechTree)
import Juralen.TechTree exposing (LevelTwo(..))
import Juralen.TechTree exposing (LevelThree(..))
import Juralen.TechTree exposing (LevelFour(..))

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
            4

        Wizard ->
            7

        Priest ->
            3

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
            1


toString : UnitType -> String
toString unitType =
    case unitType of
        Soldier ->
            "Soldier (" ++ String.fromInt (cost unitType) ++ ")"

        Warrior ->
            "Warrior (" ++ String.fromInt (cost unitType) ++ ")"

        Archer ->
            "Archer (" ++ String.fromInt (cost unitType) ++ ")"

        Knight ->
            "Knight (" ++ String.fromInt (cost unitType) ++ ")"

        Rogue ->
            "Rogue (" ++ String.fromInt (cost unitType) ++ ")"

        Wizard ->
            "Wizard (" ++ String.fromInt (cost unitType) ++ ")"

        Priest ->
            "Priest (" ++ String.fromInt (cost unitType) ++ ")"


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
            , attack = 1
            , health = 5
            , range = 0
            }

researchedUnits : TechTree -> List UnitType
researchedUnits techTree =
    case techTree.levelTwo of
        Nothing ->
            [Soldier]

        Just levelTwo ->
            let
                unitTwo = levelTwoUnit levelTwo
            in

            case techTree.levelThree of
                Nothing ->
                    [Soldier, unitTwo]

                Just levelThree ->
                    let
                        unitThree = levelThreeUnit levelThree
                    in

                    case techTree.levelFour of
                        Nothing ->
                            [Soldier, unitTwo, unitThree]

                        Just levelFour ->
                            [Soldier, unitTwo, unitThree, levelFourUnit levelFour]

levelTwoUnit : LevelTwo -> UnitType
levelTwoUnit level =
    case level of 
        BuildWarriors ->
            Warrior

        BuildArchers ->
            Archer

levelThreeUnit : LevelThree -> UnitType
levelThreeUnit level =
    case level of
        BuildKnights ->
            Knight

        BuildRogues ->
            Rogue

levelFourUnit : LevelFour -> UnitType
levelFourUnit level =
    case level of
        BuildWizards ->
            Wizard

        BuildPriests ->
            Priest