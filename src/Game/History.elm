module Game.History exposing (..)

import Game.Action as Action exposing (Action)
import Game.Loc as Loc exposing (Loc)
import Game.Structure as Structure
import Game.UnitType as UnitType
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


type alias History =
    { player : String
    , turn : Int
    , loc : Loc
    , action : Action
    }


decoder : Decoder History
decoder =
    Decode.succeed History
        |> Decode.required "player" Decode.string
        |> Decode.required "turn" Decode.int
        |> Decode.required "loc" Loc.decoder
        |> Decode.required "action" Action.decoder


encoder : History -> Encode.Value
encoder history =
    Encode.object
        [ ( "player", Encode.string history.player )
        , ( "turn", Encode.int history.turn )
        , ( "loc", Loc.encoder history.loc )
        , ( "action", Action.encoder history.action )
        ]


toString : History -> String
toString history =
    case history.action of
        Action.Move units loc ->
            history.player
                ++ " moved "
                ++ (List.length units |> String.fromInt)
                ++ " units from "
                ++ (Loc.getX history.loc |> String.fromInt)
                ++ ","
                ++ (Loc.getY history.loc |> String.fromInt)
                ++ " to "
                ++ (Loc.getX loc |> String.fromInt)
                ++ ","
                ++ (Loc.getY loc |> String.fromInt)

        Action.Attack units loc ->
            history.player
                ++ " attacked with "
                ++ (List.length units |> String.fromInt)
                ++ " units from "
                ++ (Loc.getX history.loc |> String.fromInt)
                ++ ","
                ++ (Loc.getY history.loc |> String.fromInt)
                ++ " to "
                ++ (Loc.getX loc |> String.fromInt)
                ++ ","
                ++ (Loc.getY loc |> String.fromInt)

        Action.BuildUnit unitType ->
            history.player
                ++ " built a "
                ++ UnitType.toString unitType { showCost = False }
                ++ " in "
                ++ (Loc.getX history.loc |> String.fromInt)
                ++ ","
                ++ (Loc.getY history.loc |> String.fromInt)

        Action.BuildStructure structure ->
            history.player
                ++ " built a "
                ++ Structure.toString structure
                ++ " in "
                ++ (Loc.getX history.loc |> String.fromInt)
                ++ ","
                ++ (Loc.getY history.loc |> String.fromInt)

        Action.Research techDescription ->
            history.player
                ++ " researched how to "
                ++ techDescription.name

        Action.Upgrade upgradeType ->
            history.player
                ++ (case upgradeType of
                        Action.BuildFarm ->
                            " built a farm"

                        Action.BuildTower ->
                            " built a tower"

                        Action.RepairDefense ->
                            " repaired defense"
                   )
                ++ " in "
                ++ (Loc.getX history.loc |> String.fromInt)
                ++ ","
                ++ (Loc.getY history.loc |> String.fromInt)
