module Juralen.Analysis exposing (..)

import Juralen.Cell exposing (Loc)
import Juralen.Structure exposing (Structure)
import Juralen.Unit exposing (Unit)
import Juralen.UnitType exposing (UnitType)


type Action
    = Move (List Unit) Loc
    | BuildUnit UnitType
    | BuildStructure Structure


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

        BuildUnit unitType ->
            "Build [ " ++ Juralen.UnitType.toString unitType ++ " ] in " ++ String.fromInt option.loc.x ++ ", " ++ String.fromInt option.loc.y

        _ ->
            ""
