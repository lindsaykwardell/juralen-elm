module Game.Analyzer exposing (analyze)

import Game.Action as Action exposing (UpgradeType)
import Game.AnalyzerMode exposing (AnalyzerMode(..))
import Game.Cell exposing (Cell)
import Game.CellType
import Game.Core as Core exposing (PlayerStats, allCellsInRange)
import Game.Grid
import Game.Loc as Loc exposing (Loc)
import Game.Option exposing (Option)
import Game.Player exposing (Player)
import Game.Structure as Structure
import Game.TechTree as TechTree exposing (TechLevel(..))
import Game.Unit exposing (Unit)
import Game.UnitType exposing (UnitType)
import List.Extra as List
import Sort
import Sort.Dict



-- 1. Get options
-- 1.1 Find movement options            // analyzeMoves : Core.Model -> List Option
-- 1.2 Find units to build              // analyzeBuildUnits : Core.Model -> List Option
-- 1.3 Find research options            // analyzeResearchOptions : Core.Model -> List Option
-- 1.4 Upgrade structures               // analyzeUpgrades : Core.Model -> List Option
-- 2. Score options                     // scoreOptions : List Option -> List Option
-- 3. Sort options by score             // sortOptions : List Option -> List Option
-- 4. Filter undesireable options       // filterBadOptions : List Option -> List Option
-- 5. Return Maybe Option (List.head)   // getBestOption : List Option -> Maybe Option


analyze : Core.Model -> Maybe Option
analyze model =
    analyzeBuildUnits model
        ++ analyzeMoves model
        ++ analyzeResearchOptions model
        ++ analyzeUpgrades model
        |> scoreOptions model
        |> List.filter (\option -> option.score > 0)
        |> sortByScore
        |> List.head


analyzeUpgrades : Core.Model -> List Option
analyzeUpgrades model =
    let
        targetCells : List Cell
        targetCells =
            List.filter
                (\cell -> cell.controlledBy == Just model.activePlayer && cell.structure /= Structure.None)
                (model.grid |> Sort.Dict.toList |> List.map Tuple.second)

        -- levelOneTech : Maybe TechTree.LevelOne
        -- levelOneTech =
        --     Core.getPlayerTechTree model.players model.activePlayer |> .levelOne
    in
    List.concat
        [ -- if levelOneTech == Just TechTree.BuildFarms then
          checkCellForUpgrades Action.BuildFarm targetCells []

        --   else
        --     []
        -- , if levelOneTech == Just TechTree.BuildActions then
        , checkCellForUpgrades Action.BuildTower targetCells []

        --   else
        --     []
        , checkCellForUpgrades Action.RepairDefense targetCells []
        ]


checkCellForUpgrades : UpgradeType -> List Cell -> List Option -> List Option
checkCellForUpgrades upgradeType cells options =
    case cells of
        cell :: remainingCells ->
            case upgradeType of
                Action.BuildFarm ->
                    checkCellForUpgrades upgradeType remainingCells (List.concat [ options, [ { loc = cell.loc, action = Action.Upgrade Action.BuildFarm, score = 0 } ] ])

                Action.BuildTower ->
                    checkCellForUpgrades upgradeType remainingCells (List.concat [ options, [ { loc = cell.loc, action = Action.Upgrade Action.BuildTower, score = 0 } ] ])

                Action.RepairDefense ->
                    if Structure.initDef cell.structure > cell.defBonus then
                        checkCellForUpgrades upgradeType remainingCells (List.concat [ options, [ { loc = cell.loc, action = Action.Upgrade Action.RepairDefense, score = 0 } ] ])

                    else
                        checkCellForUpgrades upgradeType remainingCells options

        [] ->
            options


analyzeResearchOptions : Core.Model -> List Option
analyzeResearchOptions model =
    Core.getPlayerTechTree model.players model.activePlayer
        |> TechTree.nextAvailableTech
        |> List.map
            (\tech ->
                { loc = Loc.at 0 0
                , action = Action.Research tech
                , score = 0
                }
            )



-- This function is returning TOO MUCH DATA at late stages of the game.
-- The goal of this refactor is to reduce the amount of data RETURNED,
-- without impacting the quality of the move analysis.
-- Fold over cellsWithUnits
-- Internally, get the unit combinations for that cell
-- Then, get move options for those combinations
-- Then, score the move options
-- Then return the highest scoring option
-- The function returns a list of one option in order to be compatible with the old code


analyzeMoves : Core.Model -> List Option
analyzeMoves model =
    let
        myUnits =
            Game.Unit.controlledBy model.units model.activePlayer
                |> List.filter (\unit -> unit.movesLeft > 0)

        cellsWithUnits : List Cell
        cellsWithUnits =
            model.grid
                |> Sort.Dict.toList
                |> List.map Tuple.second
                |> List.foldl
                    (\cell cells ->
                        if Game.Unit.inCell myUnits cell.loc /= [] then
                            cell :: cells

                        else
                            cells
                    )
                    []

        cellsInRangeSorter : Sort.Sorter ( Loc, Int )
        cellsInRangeSorter =
            Sort.custom
                (\( _, unitCountA ) ( _, unitCountB ) ->
                    if unitCountA > unitCountB then
                        GT

                    else if unitCountA < unitCountB then
                        LT

                    else
                        EQ
                )

        cellsInRangeDict : Sort.Dict.Dict ( Loc, Int ) (List Cell)
        cellsInRangeDict =
            Sort.Dict.empty cellsInRangeSorter
    in
    List.concatMap
        (\cell ->
            List.subsequences (Game.Unit.inCell myUnits cell.loc)
                |> List.foldl
                    (\units combinations ->
                        combinations ++ [ { unitOptions = units, cell = cell } ]
                    )
                    []
                |> List.foldl
                    (\combination ( combinations, cellsInRange ) ->
                        ( combination :: combinations
                        , case
                            Sort.Dict.get
                                ( combination.cell.loc
                                , List.length combination.unitOptions
                                )
                                cellsInRange
                          of
                            Nothing ->
                                allCellsInRange
                                    { model
                                        | selectedCell = combination.cell.loc
                                        , selectedUnits = List.map (\unit -> unit.id) combination.unitOptions
                                    }
                                    |> (\cells ->
                                            Sort.Dict.insert
                                                ( combination.cell.loc
                                                , List.length combination.unitOptions
                                                )
                                                cells
                                                cellsInRange
                                       )

                            Just _ ->
                                cellsInRange
                        )
                    )
                    ( [], cellsInRangeDict )
                |> (\( combinations, cellsInRange ) ->
                        List.concatMap
                            (\combination ->
                                getMoveOptions
                                    (Sort.Dict.get
                                        ( combination.cell.loc
                                        , List.length combination.unitOptions
                                        )
                                        cellsInRange
                                        |> Maybe.withDefault []
                                    )
                                    combination.cell.loc
                                    combination.unitOptions
                            )
                            combinations
                   )
        )
        cellsWithUnits


getMoveOptions : List Cell -> Loc -> List Unit -> List Option
getMoveOptions cellsInRange fromLoc units =
    List.map
        (\cell ->
            { loc = fromLoc
            , action = Action.Move units cell.loc
            , score = 0
            }
        )
        cellsInRange



-- List.foldl
--     (\unit cost ->
--         cost + Game.UnitType.moveCost unit.unitType
--     )
--     0
--     units


analyzeBuildUnits : Core.Model -> List Option
analyzeBuildUnits model =
    let
        cellsWithStructures : List Cell
        cellsWithStructures =
            model.grid
                |> Sort.Dict.toList
                |> List.map Tuple.second
                |> List.filter
                    (\cell ->
                        cell.structure
                            /= Structure.None
                            && (case cell.controlledBy of
                                    Nothing ->
                                        False

                                    Just controlledBy ->
                                        controlledBy == model.activePlayer
                               )
                    )
    in
    getUnitOptions (Core.currentPlayerStats model) cellsWithStructures []


getUnitOptions : PlayerStats -> List Cell -> List Option -> List Option
getUnitOptions stats cells options =
    case cells of
        [] ->
            options

        cell :: remainingCells ->
            let
                newOptions =
                    options
                        ++ List.map
                            (\unitType ->
                                { loc = cell.loc, action = Action.BuildUnit unitType, score = 0 }
                            )
                            (List.filter
                                (\unitType ->
                                    Game.UnitType.cost unitType <= stats.gold && stats.units < stats.farms ()
                                )
                                (Structure.canBuild cell.structure stats.techTree)
                            )
            in
            getUnitOptions stats remainingCells newOptions


scoreOptions : Core.Model -> List Option -> List Option
scoreOptions model options =
    let
        analyzer =
            Core.playerAnalyzer model.players model.activePlayer

        stats =
            Core.currentPlayerStats model

        rank =
            Core.getPlayerRanking (Core.getPlayerRankings model) model.activePlayer 1

        isTopHalf =
            round ((toFloat rank / toFloat (List.length model.players)) * 100) < 50

        isBottomHalf =
            round ((toFloat rank / toFloat (List.length model.players)) * 100) >= 50

        livingPlayers =
            List.filter (\player -> not <| Game.Grid.townCountControlledBy model.grid player.id <= 0) model.players
    in
    List.map
        (scoreOption model
            { analyzer = analyzer
            , stats = stats
            , isTopHalf = isTopHalf
            , isBottomHalf = isBottomHalf
            , livingPlayers = livingPlayers
            }
        )
        options


scoreOption :
    Core.Model
    ->
        { analyzer : AnalyzerMode
        , stats : PlayerStats
        , isTopHalf : Bool
        , isBottomHalf : Bool
        , livingPlayers : List Player
        }
    -> Option
    -> Option
scoreOption model { analyzer, stats, isTopHalf, isBottomHalf, livingPlayers } option =
    if List.length livingPlayers <= 1 then
        { option | score = -1000 }

    else
        case option.action of
            Action.Move units toLoc ->
                let
                    targetCell =
                        Sort.Dict.get toLoc model.grid
                            |> Maybe.withDefault Game.Cell.empty

                    isAttack =
                        case targetCell.controlledBy of
                            Nothing ->
                                False

                            Just playerId ->
                                playerId /= model.activePlayer

                    score =
                        10
                            -- Subtract number of units to move * 2 (fewer units is better)
                            - List.length units
                            * (if analyzer == Defensive then
                                3

                               else
                                2
                              )
                            -- Don't move units without moves left
                            - (if List.any (\unit -> unit.movesLeft <= 0) units then
                                1000

                               else
                                0
                              )
                            -- Subtract distance (shorter is better)
                            - Loc.getDistance option.loc toLoc
                            * (if analyzer == Defensive || analyzer == Expansionist then
                                3

                               else if isAttack then
                                2

                               else
                                1
                              )
                            -- Add based on cell type (plains are better than forests)
                            -- Don't move to mountains
                            + (case targetCell.cellType of
                                Game.CellType.Plains ->
                                    15

                                Game.CellType.Mountain ->
                                    -1000

                                _ ->
                                    0
                              )
                            -- Add if cell has a structure (capturing structures is better)
                            + (if targetCell.structure /= Structure.None then
                                case targetCell.controlledBy of
                                    Nothing ->
                                        100

                                    Just playerId ->
                                        if playerId == model.activePlayer then
                                            if List.length (Game.Unit.inCell model.units targetCell.loc) <= 1 then
                                                120

                                            else
                                                0

                                        else if analyzer == Aggressive then
                                            125

                                        else
                                            75

                               else if analyzer == Aggressive then
                                100

                               else
                                0
                              )
                            -- Is it worth moving there?
                            -- No impact if cell is not controlled.
                            -- If analyzer is expansionist, then prefer uncontrolled.
                            -- Negative if it is controller by active player
                            -- Add if controlled by someone else (capturing enemy territory is better)
                            -- Analyze threat of enemy units before making a decision, attacking is less important than winning
                            -- Include defensive bonus of cell before attacking
                            + (case targetCell.controlledBy of
                                Nothing ->
                                    let
                                        isForest =
                                            targetCell.cellType == Game.CellType.Forest
                                    in
                                    if isForest then
                                        0

                                    else if analyzer == Expansionist then
                                        600

                                    else
                                        300

                                Just playerId ->
                                    if playerId == model.activePlayer then
                                        -- -10
                                        -100

                                    else if Game.Grid.townCountControlledBy model.grid playerId == 0 then
                                        -- if the player has no towns, don't bother attacking (you'll inherit their stuff, no need to kill it)
                                        -1000

                                    else
                                        abs (Core.getPlayerScore model model.activePlayer - Core.getPlayerScore model playerId)
                                            + (if List.length (Game.Unit.inCell model.units targetCell.loc) > 0 then
                                                if analyzer == Passive then
                                                    -1000

                                                else
                                                    let
                                                        attackerThreat =
                                                            List.foldl (\unit threat -> threat + Game.UnitType.threat unit)
                                                                0
                                                                (List.map (\unit -> unit.unitType) units)

                                                        defenderThreat =
                                                            List.foldl (\unit threat -> threat + Game.UnitType.threat unit)
                                                                0
                                                                (List.map (\unit -> unit.unitType) (Game.Unit.inCell model.units targetCell.loc))
                                                    in
                                                    -- Don't attack if you're weaker than the enemy
                                                    -- Doesn't account for everything, but that's intentional
                                                    if attackerThreat < defenderThreat then
                                                        -1000

                                                    else
                                                        let
                                                            defBonus =
                                                                targetCell.defBonus
                                                        in
                                                        attackerThreat - defenderThreat - defBonus

                                               else if analyzer == Aggressive then
                                                100

                                               else
                                                50
                                              )
                              )

                    -- Should we move everyone in this cell? Do we really need more farms?
                    -- Defending territory is preferable if we can get away with it
                    -- + (if List.length (Game.Unit.inCell model.units option.loc) <= List.length units then
                    --     if stats.farms == stats.units then
                    --         10
                    --     else
                    --         -1000
                    --    else
                    --     0
                    --   )
                in
                { option
                    | score = score
                    , action =
                        if isAttack then
                            Action.Attack units toLoc

                        else
                            Action.Move units toLoc
                }

            Action.BuildUnit unitType ->
                let
                    unitsInCell =
                        List.length (Game.Unit.inCell model.units option.loc)

                    score =
                        -- Build based on priority (higher scores are better)
                        1
                            + unitTypeScore unitType
                            - round (toFloat (List.length (List.filter (\unit -> unit.unitType == unitType && unit.controlledBy == model.activePlayer) model.units)) / 2)
                            -- Build based on current unit count in cell (lower pop of units is more important)
                            + (if unitsInCell <= 0 then
                                1000

                               else
                                5 - unitsInCell
                              )
                in
                { option | score = score }

            Action.BuildStructure _ ->
                option

            Action.Research research ->
                if stats.gold <= research.cost || stats.farms () == stats.units || analyzer == Passive then
                    { option | score = -1000 }

                else
                    case research.tech of
                        -- LevelOne tech ->
                        -- case tech of
                        --     TechTree.BuildFarms ->
                        --         { option
                        --             | score =
                        --                 if analyzer == Aggressive || analyzer == Expansionist then
                        --                     1000
                        --                 else
                        --                     150 + (stats.units * 10) - (stats.farms * 10)
                        --         }
                        --     TechTree.BuildActions ->
                        --         { option
                        --             | score =
                        --                 if analyzer == Default then
                        --                     1000
                        --                 else
                        --                     150 + (50 - (stats.towns * 10) - (stats.units * 10))
                        --         }
                        LevelTwo tech ->
                            case tech of
                                TechTree.BuildArchers ->
                                    { option
                                        | score =
                                            200
                                                + (if isBottomHalf then
                                                    200

                                                   else if analyzer == Aggressive then
                                                    200

                                                   else
                                                    0
                                                  )
                                    }

                                TechTree.BuildWarriors ->
                                    { option
                                        | score =
                                            200
                                                + (if isTopHalf then
                                                    200

                                                   else if analyzer == Defensive then
                                                    200

                                                   else
                                                    0
                                                  )
                                    }

                        LevelThree tech ->
                            case tech of
                                TechTree.BuildKnights ->
                                    { option
                                        | score =
                                            200
                                                + (if analyzer == Aggressive || analyzer == Expansionist then
                                                    1000

                                                   else if isTopHalf then
                                                    500

                                                   else
                                                    0
                                                  )
                                    }

                                TechTree.BuildRogues ->
                                    { option
                                        | score =
                                            200
                                                + (if not isTopHalf then
                                                    300

                                                   else if analyzer == Defensive then
                                                    300

                                                   else
                                                    0
                                                  )
                                    }

                        LevelFour tech ->
                            case tech of
                                TechTree.BuildWizards ->
                                    { option
                                        | score =
                                            500
                                                + (if analyzer == Expansionist || stats.gold > 25 then
                                                    1000

                                                   else if analyzer == Default then
                                                    1000

                                                   else
                                                    0
                                                  )
                                    }

                                TechTree.BuildPriests ->
                                    { option
                                        | score =
                                            500
                                                + (if stats.farms () > 15 then
                                                    500

                                                   else if analyzer == Defensive then
                                                    1000

                                                   else
                                                    0
                                                  )
                                    }

            Action.Upgrade upgradeType ->
                let
                    cell =
                        Sort.Dict.get option.loc model.grid |> Maybe.withDefault Game.Cell.empty
                in
                case upgradeType of
                    Action.RepairDefense ->
                        { option
                            | score =
                                if stats.gold < 1 then
                                    -1000

                                else if stats.units < round (toFloat (stats.farms ()) / 2) then
                                    -100

                                else if cell.defBonus <= 0 then
                                    300

                                else if analyzer /= Aggressive then
                                    150

                                else
                                    25
                        }

                    Action.BuildFarm ->
                        { option
                            | score =
                                if stats.gold < 2 || (cell.farms + cell.towers) >= Structure.maxUpgradeCount cell.structure then
                                    -1000

                                else
                                    1
                                        - cell.farms
                                        + Game.Grid.distanceToEnemy model.grid option.loc model.activePlayer
                                        + (if analyzer == Defensive then
                                            2

                                           else if analyzer == Passive then
                                            5

                                           else
                                            0
                                          )
                        }

                    Action.BuildTower ->
                        { option
                            | score =
                                if stats.gold < 2 || (cell.farms + cell.towers) >= Structure.maxUpgradeCount cell.structure then
                                    -1000

                                else
                                    1
                                        - cell.towers
                                        + Game.Grid.distanceToEnemy model.grid option.loc model.activePlayer
                                        + (if analyzer == Aggressive || analyzer == Expansionist then
                                            2

                                           else
                                            0
                                          )
                        }

            _ ->
                option


unitTypeScore : UnitType -> Int
unitTypeScore unitType =
    case unitType of
        Game.UnitType.Soldier ->
            1

        Game.UnitType.Warrior ->
            2

        Game.UnitType.Archer ->
            2

        Game.UnitType.Priest ->
            4

        Game.UnitType.Rogue ->
            3

        Game.UnitType.Wizard ->
            4

        Game.UnitType.Knight ->
            3


sortByScore : List Option -> List Option
sortByScore options =
    List.sortBy .score options |> List.reverse
