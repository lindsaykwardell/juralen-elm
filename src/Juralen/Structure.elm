module Juralen.Structure exposing (..)

import Juralen.TechTree exposing (TechTree)
import Juralen.UnitType exposing (UnitType(..))


type Structure
    = Town
    | Citadel
    | None


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
                    5

                _ ->
                    0


canBuild : Maybe Structure -> TechTree -> List UnitType
canBuild structure techTree =
    case structure of
        Nothing ->
            []

        Just real ->
            if real == Town then
                [ Soldier ]

            else
                Juralen.UnitType.researchedUnits techTree


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

                None ->
                    ""


getCellClass : Maybe Structure -> String
getCellClass structure =
    case structure of
        Nothing ->
            ""

        Just real ->
            case real of
                Town ->
                    "town"

                Citadel ->
                    "citadel"

                None ->
                    ""
