module Juralen.Structure exposing (..)

import Juralen.UnitType exposing (UnitType, UnitType(..))

type Structure
    = Town
    | Citadel
    | Castle
    | Academy
    | Temple
    | City
    | Lodge


initDef : Maybe Structure -> Int
initDef structure = 
    case structure of
        Nothing ->
            0

        Just real ->
            case real of
                Town ->
                    3
                
                Citadel ->
                    7

                _ ->
                    5


canBuild : Maybe Structure -> List UnitType
canBuild structure =
    case structure of
        Nothing ->
            []

        Just real ->
            case real of
                Town ->
                    [Soldier]

                Citadel ->
                    [Soldier, Warrior]

                Castle ->
                    [Soldier, Knight]

                Academy ->
                    [Soldier, Wizard]

                Temple ->
                    [Soldier, Priest]

                City ->
                    [Soldier, Rogue]

                Lodge ->
                    [Soldier, Archer]


toString : Maybe Structure -> String
toString structure =
    case structure of
        Nothing ->
          ""

        Just real ->
            case real of
                Town ->
                    "Town"

                Citadel ->
                    "Citadel"

                Castle ->
                    "Castle"

                Academy ->
                    "Academy"

                Temple ->
                    "Temple"

                City ->
                    "City"

                Lodge ->
                    "Lodge"