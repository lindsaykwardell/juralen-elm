module Game.Structure exposing (..)

import Game.TechTree exposing (TechTree)
import Game.UnitType exposing (UnitType(..))


type Structure
    = Town
    | Citadel
    | None


initDef : Structure -> Int
initDef structure =
    case structure of
        Town ->
            3

        Citadel ->
            5

        _ ->
            0


canBuild : Structure -> TechTree -> List UnitType
canBuild structure techTree =
    case structure of
        None ->
            []

        Town ->
            [ Soldier ]

        Citadel ->
            Game.UnitType.researchedUnits techTree


toString : Structure -> String
toString structure =
    case structure of
        Town ->
            "Town"

        Citadel ->
            "Citadel"

        None ->
            ""


getCellClass : Structure -> String
getCellClass structure =
    case structure of
        Town ->
            "town"

        Citadel ->
            "citadel"

        None ->
            ""
