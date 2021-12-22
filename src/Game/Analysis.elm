module Game.Analysis exposing (..)

import Game.Loc as Loc exposing (Loc)
import Game.Structure exposing (Structure)
import Game.TechTree exposing (TechDescription)
import Game.Unit exposing (Unit)
import Game.UnitType exposing (UnitType)


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
                    List.foldl (\unit total -> total ++ Game.UnitType.toString unit.unitType { showCost = False } ++ " ") "" units
            in
            "Move [ "
                ++ unitString
                ++ "] from "
                ++ String.fromInt (Loc.getX option.loc)
                ++ ", "
                ++ String.fromInt (Loc.getY option.loc)
                ++ " to "
                ++ String.fromInt (Loc.getX loc)
                ++ ", "
                ++ String.fromInt (Loc.getY loc)

        Attack units loc ->
            let
                unitString : String
                unitString =
                    List.foldl (\unit total -> total ++ Game.UnitType.toString unit.unitType { showCost = False } ++ " ") "" units
            in
            "Attack - Move [ "
                ++ unitString
                ++ "] from "
                ++ String.fromInt (Loc.getX option.loc)
                ++ ", "
                ++ String.fromInt (Loc.getY option.loc)
                ++ " to "
                ++ String.fromInt (Loc.getX loc)
                ++ ", "
                ++ String.fromInt (Loc.getY loc)

        BuildUnit unitType ->
            "Build [ "
                ++ Game.UnitType.toString unitType { showCost = True }
                ++ " ] in "
                ++ String.fromInt (Loc.getX option.loc)
                ++ ", "
                ++ String.fromInt (Loc.getY option.loc)

        Research tech ->
            "Research " ++ tech.name

        Upgrade upgrade ->
            case upgrade of
                BuildFarm ->
                    "Build farm in "
                        ++ String.fromInt (Loc.getX option.loc)
                        ++ ", "
                        ++ String.fromInt (Loc.getY option.loc)

                BuildTower ->
                    "Build tower in "
                        ++ String.fromInt (Loc.getX option.loc)
                        ++ ", "
                        ++ String.fromInt (Loc.getY option.loc)

                RepairDefense ->
                    "Repair structure in "
                        ++ String.fromInt (Loc.getX option.loc)
                        ++ ", "
                        ++ String.fromInt (Loc.getY option.loc)

        _ ->
            ""
