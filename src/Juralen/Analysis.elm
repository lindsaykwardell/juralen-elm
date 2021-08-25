module Juralen.Analysis exposing (..)

import Juralen.Cell exposing (Loc)
import Juralen.Structure exposing (Structure)
import Juralen.TechTree exposing (TechDescription)
import Juralen.Unit exposing (Unit)
import Juralen.UnitType exposing (UnitType)


type Action
    = Move (List Unit) Loc
    | Attack (List Unit) Loc
    | BuildUnit UnitType
    | BuildStructure Structure
    | Research TechDescription
    | Upgrade UpgradeType


type UpgradeType
    = BuildFarm
    | BuildTower
    | RepairDefense


type alias Option =
    { loc : Loc
    , action : Action
    , score : Int
    }


toString : Option -> String
toString option =
    case option.action of
        Move units loc ->
            let
                unitString : String
                unitString =
                    List.foldl (\unit total -> total ++ Juralen.UnitType.toString unit.unitType ++ " ") "" units
            in
            "Move [ " ++ unitString ++ "] from " ++ String.fromInt option.loc.x ++ ", " ++ String.fromInt option.loc.y ++ " to " ++ String.fromInt loc.x ++ ", " ++ String.fromInt loc.y

        Attack units loc ->
            let
                unitString : String
                unitString =
                    List.foldl (\unit total -> total ++ Juralen.UnitType.toString unit.unitType ++ " ") "" units
            in
            "Attack - Move [ " ++ unitString ++ "] from " ++ String.fromInt option.loc.x ++ ", " ++ String.fromInt option.loc.y ++ " to " ++ String.fromInt loc.x ++ ", " ++ String.fromInt loc.y

        BuildUnit unitType ->
            "Build [ " ++ Juralen.UnitType.toString unitType ++ " ] in " ++ String.fromInt option.loc.x ++ ", " ++ String.fromInt option.loc.y

        Research tech ->
            "Research " ++ tech.name

        Upgrade upgrade ->
            case upgrade of
                BuildFarm ->
                    "Build farm in " ++ String.fromInt option.loc.x ++ ", " ++ String.fromInt option.loc.y

                BuildTower ->
                    "Build tower in " ++ String.fromInt option.loc.x ++ ", " ++ String.fromInt option.loc.y

                RepairDefense ->
                    "Repair structure in " ++ String.fromInt option.loc.x ++ ", " ++ String.fromInt option.loc.y

        _ ->
            ""
