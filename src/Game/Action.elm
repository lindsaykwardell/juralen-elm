module Game.Action exposing (Action(..), UpgradeType(..), decoder, encoder)

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


decoder : Decoder Action
decoder =
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
                                        case ( Decode.decodeString (Decode.list Game.Unit.decoder) unitString, Loc.fromString locString ) of
                                            ( Ok units, Ok loc ) ->
                                                Decode.succeed (Move units loc)

                                            ( Err err, Ok _ ) ->
                                                Decode.fail <| "Invalid move" ++ Decode.errorToString err

                                            ( Ok _, Err err ) ->
                                                Decode.fail <| "Invalid loc" ++ err

                                            _ ->
                                                Decode.fail "Invalid move"

                            "attack" ->
                                case ( Dict.get "units" dict, Dict.get "loc" dict ) of
                                    ( Nothing, _ ) ->
                                        Decode.fail "No units"

                                    ( _, Nothing ) ->
                                        Decode.fail "No loc"

                                    ( Just unitString, Just locString ) ->
                                        case ( Decode.decodeString (Decode.list Game.Unit.decoder) unitString, Decode.decodeString Loc.decoder (locString |> Encode.string |> Encode.encode 0) ) of
                                            ( Ok units, Ok loc ) ->
                                                Decode.succeed (Attack units loc)

                                            _ ->
                                                Decode.fail "Invalid attack"

                            "buildUnit" ->
                                case Dict.get "unitType" dict of
                                    Nothing ->
                                        Decode.fail "No unitType"

                                    Just unitTypeString ->
                                        case Decode.decodeString Game.UnitType.decoder (unitTypeString |> Encode.string |> Encode.encode 0) of
                                            Ok unitType ->
                                                Decode.succeed (BuildUnit unitType)

                                            _ ->
                                                Decode.fail "Invalid unitType"

                            "buildStructure" ->
                                case Dict.get "structure" dict of
                                    Nothing ->
                                        Decode.fail "No structure"

                                    Just structureString ->
                                        case Decode.decodeString Game.Structure.decoder (structureString |> Encode.string |> Encode.encode 0) of
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

                                            Err err ->
                                                Decode.fail (Decode.errorToString err)

                            "upgrade" ->
                                case Dict.get "upgradeType" dict of
                                    Nothing ->
                                        Decode.fail "No upgradeType"

                                    Just upgradeTypeString ->
                                        case Decode.decodeString upgradeTypeDecoder (upgradeTypeString |> Encode.string |> Encode.encode 0) of
                                            Ok upgradeType ->
                                                Decode.succeed (Upgrade upgradeType)

                                            _ ->
                                                Decode.fail "Invalid upgradeType"

                            _ ->
                                Decode.fail "Invalid action"
            )


encoder : Action -> Encode.Value
encoder action =
    case action of
        Move units loc ->
            Encode.object
                [ ( "action", Encode.string "move" )
                , ( "units", Encode.list Game.Unit.encoder units |> Encode.encode 0 |> Encode.string )
                , ( "loc", Loc.encoder loc )
                ]

        Attack units loc ->
            Encode.object
                [ ( "action", Encode.string "attack" )
                , ( "units", Encode.list Game.Unit.encoder units |> Encode.encode 0 |> Encode.string )
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
                , ( "tech", Game.TechTree.techDescriptionEncoder tech |> Encode.encode 0 |> Encode.string )
                ]

        Upgrade upgradeType ->
            Encode.object
                [ ( "action", Encode.string "upgrade" )
                , ( "upgradeType", upgradeTypeEncoder upgradeType )
                ]


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
