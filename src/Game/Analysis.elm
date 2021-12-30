module Game.Analysis exposing (..)

import Dict
import Game.Loc as Loc exposing (Loc)
import Game.Structure exposing (Structure)
import Game.TechTree exposing (TechDescription)
import Game.Unit exposing (Unit)
import Game.UnitType exposing (UnitType)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


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


upgradeTypeDecoder : Decoder UpgradeType
upgradeTypeDecoder =
    Decode.string
        |> Decode.andThen
            (\upgradeType ->
                case upgradeType of
                    "buildFarm" ->
                        Decode.succeed BuildFarm

                    "buildTower" ->
                        Decode.succeed BuildTower

                    "repairDefense" ->
                        Decode.succeed RepairDefense

                    _ ->
                        Decode.fail "Invalid upgrade type"
            )


upgradeTypeEncoder : UpgradeType -> Encode.Value
upgradeTypeEncoder upgradeType =
    case upgradeType of
        BuildFarm ->
            Encode.string "buildFarm"

        BuildTower ->
            Encode.string "buildTower"

        RepairDefense ->
            Encode.string "repairDefense"


type alias Option =
    { loc : Loc
    , action : Action
    , score : Int
    }


decoder : Decoder Option
decoder =
    Decode.succeed Option
        |> Decode.required "loc" Loc.decoder
        |> Decode.required "action" actionDecoder
        |> Decode.required "score" Decode.int


encoder : Option -> Encode.Value
encoder option =
    Encode.object
        [ ( "loc", Loc.encoder option.loc )
        , ( "action", actionEncoder option.action )
        , ( "score", Encode.int option.score )
        ]


actionDecoder : Decoder Action
actionDecoder =
    Decode.string
        |> Decode.dict
        |> Decode.andThen
            (\dict ->
                case Dict.get "action" dict of
                    Nothing ->
                        Decode.fail "No action"

                    Just action ->
                        case action of
                            "move" ->
                                case ( Dict.get "units" dict, Dict.get "loc" dict ) of
                                    ( Nothing, _ ) ->
                                        Decode.fail "No units"

                                    ( _, Nothing ) ->
                                        Decode.fail "No loc"

                                    ( Just unitString, Just locString ) ->
                                        case ( Decode.decodeString (Decode.list Game.Unit.decoder) unitString, Decode.decodeString Loc.decoder locString ) of
                                            ( Ok units, Ok loc ) ->
                                                Decode.succeed (Move units loc)

                                            _ ->
                                                Decode.fail "Invalid move"

                            "attack" ->
                                case ( Dict.get "units" dict, Dict.get "loc" dict ) of
                                    ( Nothing, _ ) ->
                                        Decode.fail "No units"

                                    ( _, Nothing ) ->
                                        Decode.fail "No loc"

                                    ( Just unitString, Just locString ) ->
                                        case ( Decode.decodeString (Decode.list Game.Unit.decoder) unitString, Decode.decodeString Loc.decoder locString ) of
                                            ( Ok units, Ok loc ) ->
                                                Decode.succeed (Attack units loc)

                                            _ ->
                                                Decode.fail "Invalid attack"

                            "buildUnit" ->
                                case Dict.get "unitType" dict of
                                    Nothing ->
                                        Decode.fail "No unitType"

                                    Just unitTypeString ->
                                        case Decode.decodeString Game.UnitType.decoder unitTypeString of
                                            Ok unitType ->
                                                Decode.succeed (BuildUnit unitType)

                                            _ ->
                                                Decode.fail "Invalid unitType"

                            "buildStructure" ->
                                case Dict.get "structure" dict of
                                    Nothing ->
                                        Decode.fail "No structure"

                                    Just structureString ->
                                        case Decode.decodeString Game.Structure.decoder structureString of
                                            Ok structure ->
                                                Decode.succeed (BuildStructure structure)

                                            _ ->
                                                Decode.fail "Invalid structure"

                            "research" ->
                                case Dict.get "tech" dict of
                                    Nothing ->
                                        Decode.fail "No tech"

                                    Just techString ->
                                        case Decode.decodeString Game.TechTree.techDescriptionDecoder techString of
                                            Ok tech ->
                                                Decode.succeed (Research tech)

                                            _ ->
                                                Decode.fail "Invalid tech"

                            "upgrade" ->
                                case Dict.get "upgradeType" dict of
                                    Nothing ->
                                        Decode.fail "No upgradeType"

                                    Just upgradeTypeString ->
                                        case Decode.decodeString upgradeTypeDecoder upgradeTypeString of
                                            Ok upgradeType ->
                                                Decode.succeed (Upgrade upgradeType)

                                            _ ->
                                                Decode.fail "Invalid upgradeType"

                            _ ->
                                Decode.fail "Invalid action"
            )


actionEncoder : Action -> Encode.Value
actionEncoder action =
    case action of
        Move units loc ->
            Encode.object
                [ ( "action", Encode.string "move" )
                , ( "units", Encode.list Game.Unit.encoder units )
                , ( "loc", Loc.encoder loc )
                ]

        Attack units loc ->
            Encode.object
                [ ( "action", Encode.string "attack" )
                , ( "units", Encode.list Game.Unit.encoder units )
                , ( "loc", Loc.encoder loc )
                ]

        BuildUnit unitType ->
            Encode.object
                [ ( "action", Encode.string "buildUnit" )
                , ( "unitType", Game.UnitType.encoder unitType )
                ]

        BuildStructure structure ->
            Encode.object
                [ ( "action", Encode.string "buildStructure" )
                , ( "structure", Game.Structure.encoder structure )
                ]

        Research tech ->
            Encode.object
                [ ( "action", Encode.string "research" )
                , ( "tech", Game.TechTree.techDescriptionEncoder tech )
                ]

        Upgrade upgradeType ->
            Encode.object
                [ ( "action", Encode.string "upgrade" )
                , ( "upgradeType", upgradeTypeEncoder upgradeType )
                ]


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
