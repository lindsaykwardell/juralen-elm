module Game.History exposing (History, decoder, encoder, favoriteUnit, leastAggressivePlayer, mostAggressivePlayer, mostCommonResearch, toString, totalCombats, unitsBuilt)

import Dict
import Game.Action as Action exposing (Action)
import Game.Loc as Loc exposing (Loc)
import Game.Player exposing (Player)
import Game.PlayerScore exposing (PlayerScore)
import Game.Structure as Structure
import Game.UnitType as UnitType exposing (UnitType(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


type alias History =
    { player : String
    , turn : Int
    , loc : Loc
    , action : Action
    , scores : List PlayerScore
    }


decoder : Decoder History
decoder =
    Decode.succeed History
        |> Decode.required "player" Decode.string
        |> Decode.required "turn" Decode.int
        |> Decode.required "loc" Loc.decoder
        |> Decode.required "action" Action.decoder
        |> Decode.required "scores"
            (Decode.list
                (Decode.succeed PlayerScore
                    |> Decode.required "playerId" Decode.int
                    |> Decode.required "score" Decode.int
                )
            )


encoder : History -> Encode.Value
encoder history =
    Encode.object
        [ ( "player", Encode.string history.player )
        , ( "turn", Encode.int history.turn )
        , ( "loc", Loc.encoder history.loc )
        , ( "action", Action.encoder history.action )
        , ( "scores"
          , Encode.list
                (\score ->
                    Encode.object
                        [ ( "playerId", Encode.int score.playerId )
                        , ( "score", Encode.int score.score )
                        ]
                )
                history.scores
          )
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
                ++ UnitType.toString unitType
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


mostAggressivePlayer : List Player -> List History -> Player
mostAggressivePlayer players history =
    let
        attacksByPlayers : List { id : Int, attacks : Int }
        attacksByPlayers =
            List.map
                (\player ->
                    { id = player.id
                    , attacks =
                        List.length
                            (List.filter
                                (\action ->
                                    case action.action of
                                        Action.Attack _ _ ->
                                            action.player == player.name

                                        _ ->
                                            False
                                )
                                history
                            )
                    }
                )
                players
    in
    List.foldl
        (\selectedPlayer player ->
            if player.attacks > selectedPlayer.attacks then
                player

            else
                selectedPlayer
        )
        { id = -1, attacks = -1 }
        attacksByPlayers
        |> .id
        |> Game.Player.get players


leastAggressivePlayer : List Player -> List History -> Player
leastAggressivePlayer players history =
    let
        attacksByPlayers : List { id : Int, attacks : Int }
        attacksByPlayers =
            List.map
                (\player ->
                    { id = player.id
                    , attacks =
                        List.length
                            (List.filter
                                (\action ->
                                    case action.action of
                                        Action.Attack _ _ ->
                                            action.player == player.name

                                        _ ->
                                            False
                                )
                                history
                            )
                    }
                )
                players
    in
    List.foldl
        (\selectedPlayer player ->
            if player.attacks < selectedPlayer.attacks then
                player

            else
                selectedPlayer
        )
        { id = -1, attacks = 1000 }
        attacksByPlayers
        |> .id
        |> Game.Player.get players



-- favoriteUnit : List History -> UnitType


favoriteUnit : List History -> String
favoriteUnit history =
    history
        |> List.foldl
            (\action unitCounts ->
                case action.action of
                    Action.BuildUnit unitType ->
                        if unitType == Soldier then
                            unitCounts

                        else
                            let
                                strType =
                                    UnitType.toString unitType
                            in
                            case Dict.get strType unitCounts of
                                Nothing ->
                                    Dict.insert strType 1 unitCounts

                                Just val ->
                                    Dict.insert strType (val + 1) unitCounts

                    _ ->
                        unitCounts
            )
            Dict.empty
        |> Dict.toList
        |> List.sortBy (\( _, v ) -> v)
        |> List.reverse
        |> List.head
        |> Maybe.map (\( k, _ ) -> k)
        |> Maybe.withDefault ""


mostCommonResearch : List History -> String
mostCommonResearch history =
    history
        |> List.foldl
            (\action researchCounts ->
                case action.action of
                    Action.Research techDescription ->
                        let
                            strType =
                                techDescription.name
                        in
                        case Dict.get strType researchCounts of
                            Nothing ->
                                Dict.insert strType 1 researchCounts

                            Just val ->
                                Dict.insert strType (val + 1) researchCounts

                    _ ->
                        researchCounts
            )
            Dict.empty
        |> Dict.toList
        |> List.sortBy (\( _, v ) -> v)
        |> List.head
        |> Maybe.map (\( k, _ ) -> k)
        |> Maybe.withDefault ""


unitsBuilt : List History -> Int
unitsBuilt history =
    history
        |> List.foldl
            (\action unitCounts ->
                case action.action of
                    Action.BuildUnit _ ->
                        unitCounts + 1

                    _ ->
                        unitCounts
            )
            0


totalCombats : List History -> Int
totalCombats history =
    history
        |> List.filter
            (\action ->
                case action.action of
                    Action.Attack _ _ ->
                        True

                    _ ->
                        False
            )
        |> List.length
