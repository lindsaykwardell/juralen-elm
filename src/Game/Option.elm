module Game.Option exposing (..)

import Game.Action as Action exposing (Action)
import Game.Loc as Loc exposing (Loc)
import Game.UnitType
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


type alias Option =
    { loc : Loc
    , action : Action
    , score : Int
    }


decoder : Decoder Option
decoder =
    Decode.succeed Option
        |> Decode.required "loc" Loc.decoder
        |> Decode.required "action" Action.decoder
        |> Decode.required "score" Decode.int


encoder : Option -> Encode.Value
encoder option =
    Encode.object
        [ ( "loc", Loc.encoder option.loc )
        , ( "action", Action.encoder option.action )
        , ( "score", Encode.int option.score )
        ]


toString : Option -> String
toString option =
    case option.action of
        Action.Move units loc ->
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

        Action.Attack units loc ->
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

        Action.BuildUnit unitType ->
            "Build [ "
                ++ Game.UnitType.toString unitType { showCost = True }
                ++ " ] in "
                ++ String.fromInt (Loc.getX option.loc)
                ++ ", "
                ++ String.fromInt (Loc.getY option.loc)

        Action.Research tech ->
            "Research " ++ tech.name

        Action.Upgrade upgrade ->
            case upgrade of
                Action.BuildFarm ->
                    "Build farm in "
                        ++ String.fromInt (Loc.getX option.loc)
                        ++ ", "
                        ++ String.fromInt (Loc.getY option.loc)

                Action.BuildTower ->
                    "Build tower in "
                        ++ String.fromInt (Loc.getX option.loc)
                        ++ ", "
                        ++ String.fromInt (Loc.getY option.loc)

                Action.RepairDefense ->
                    "Repair structure in "
                        ++ String.fromInt (Loc.getX option.loc)
                        ++ ", "
                        ++ String.fromInt (Loc.getY option.loc)

        _ ->
            ""
