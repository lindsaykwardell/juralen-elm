module Game.Analyzer exposing (..)

import Game.Analysis exposing (Action(..), Option, UpgradeType)
import Game.AnalyzerMode exposing (AnalyzerMode(..))
import Game.Cell exposing (Cell)
import Game.CellType
import Game.Core as Core exposing (Model, PlayerStats, allCellsInRange)
import Game.Grid
import Game.Loc as Loc exposing (Loc)
import Game.Structure as Structure exposing (Structure)
import Game.TechTree as TechTree exposing (TechLevel(..))
import Game.Unit exposing (Unit)
import Game.UnitType exposing (UnitType)



-- 1. Get options
-- 1.1 Find movement options            // analyzeMoves : Core.Model -> List Option
-- 1.2 Find units to build              // analyzeBuildUnits : Core.Model -> List Option
-- 1.3 Find research options            // analyzeResearchOptions : Core.Model -> List Option
-- 1.4 Upgrade structures               // analyzeUpgrades : Core.Model -> List Option
-- 2. Score options                     // scoreOptions : List Option -> List Option
-- 3. Sort options by score             // sortOptions : List Option -> List Option
-- 4. Filter undesireable options       // filterBadOptions : List Option -> List Option
-- 5. Return Maybe Option (List.head)   // getBestOption : List Option -> Maybe Option


analyze : Core.Model -> List Option
analyze model =
    analyzeBuildUnits model
        ++ analyzeMoves model
        ++ analyzeResearchOptions model
        ++ analyzeUpgrades model
        |> scoreOptions model
        |> List.filter (\option -> option.score > 0)
        |> sortByScore


type alias UnitOption =
    { unitOptions : List Unit
    , cell : Cell
    }


analyzeUpgrades : Core.Model -> List Option
analyzeUpgrades model =
    let
        targetCells : List Cell
        targetCells =
            List.filter
                (\cell -> cell.controlledBy == Just model.activePlayer && cell.structure /= Structure.None)
                (toList model.grid)

        -- levelOneTech : Maybe TechTree.LevelOne
        -- levelOneTech =
        --     Core.getPlayerTechTree model.players model.activePlayer |> .levelOne
    in
    List.concat
        [ -- if levelOneTech == Just TechTree.BuildFarms then
          checkCellForUpgrades Game.Analysis.BuildFarm targetCells []

        --   else
        --     []
        -- , if levelOneTech == Just TechTree.BuildActions then
        , checkCellForUpgrades Game.Analysis.BuildTower targetCells []

        --   else
        --     []
        , checkCellForUpgrades Game.Analysis.RepairDefense targetCells []
        ]


checkCellForUpgrades : UpgradeType -> List Cell -> List Option -> List Option
checkCellForUpgrades upgradeType cells options =
    case cells of
        cell :: remainingCells ->
            case upgradeType of
                Game.Analysis.BuildFarm ->
                    checkCellForUpgrades upgradeType remainingCells (List.concat [ options, [ { loc = cell.loc, action = Upgrade Game.Analysis.BuildFarm, score = 0 } ] ])

                Game.Analysis.BuildTower ->
                    checkCellForUpgrades upgradeType remainingCells (List.concat [ options, [ { loc = cell.loc, action = Upgrade Game.Analysis.BuildTower, score = 0 } ] ])

                Game.Analysis.RepairDefense ->
                    if Structure.initDef cell.structure > cell.defBonus then
                        checkCellForUpgrades upgradeType remainingCells (List.concat [ options, [ { loc = cell.loc, action = Upgrade Game.Analysis.RepairDefense, score = 0 } ] ])

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
                , action = Research tech
                , score = 0
                }
            )


analyzeMoves : Core.Model -> List Option
analyzeMoves model =
    let
        myUnits =
            Game.Unit.controlledBy model.units model.activePlayer

        cellsWithUnits : List Cell
        cellsWithUnits =
            model.grid
                |> List.foldl
                    (\row collection ->
                        List.foldl
                            (\cell cells ->
                                if Game.Unit.inCell myUnits cell.loc /= [] then
                                    cell :: cells

                                else
                                    cells
                            )
                            collection
                            row
                    )
                    []

        unitCombinations : List UnitOption
        unitCombinations =
            toList
                (List.map
                    (\cell ->
                        List.foldl
                            (\units combinations ->
                                combinations ++ [ { unitOptions = units, cell = cell } ]
                            )
                            []
                            (combineUnitWith [] [] (Game.Unit.inCell myUnits cell.loc))
                    )
                    cellsWithUnits
                )
    in
    toList
        (List.map
            (\combination ->
                getMoveOptions model combination.cell.loc combination.unitOptions
            )
            unitCombinations
        )


toList : List (List a) -> List a
toList grid =
    addNextRow grid []


addNextRow : List (List a) -> List a -> List a
addNextRow grid cellList =
    let
        firstRow : List a
        firstRow =
            case List.head grid of
                Nothing ->
                    []

                Just row ->
                    row

        remainingRows : List (List a)
        remainingRows =
            case List.tail grid of
                Nothing ->
                    []

                Just rows ->
                    rows

        updatedList : List a
        updatedList =
            cellList ++ firstRow
    in
    if List.length firstRow <= 0 then
        updatedList

    else
        addNextRow remainingRows updatedList


combineUnitWith : List (List Unit) -> List Unit -> List Unit -> List (List Unit)
combineUnitWith unitCombinations selectedUnits unusedUnits =
    let
        remainingUnusedUnits =
            case List.tail unusedUnits of
                Nothing ->
                    []

                Just units ->
                    units

        newCombination : List Unit
        newCombination =
            case List.head unusedUnits of
                Nothing ->
                    []

                Just unit ->
                    if List.member unit selectedUnits then
                        []

                    else
                        selectedUnits ++ [ unit ]

        newCombinations =
            if List.length newCombination > 0 then
                unitCombinations ++ [ newCombination ]

            else
                unitCombinations
    in
    if List.length newCombination <= 0 then
        newCombinations

    else
        combineUnitWith newCombinations newCombination remainingUnusedUnits


getMoveOptions : Model -> Loc -> List Unit -> List Option
getMoveOptions model fromLoc units =
    let
        cellsInRange =
            allCellsInRange
                { model
                    | selectedCell = fromLoc
                    , selectedUnits = List.map (\unit -> unit.id) units
                }
    in
    List.map
        (\cell ->
            { loc = fromLoc
            , action = Move units cell.loc
            , score = 0
            }
        )
        cellsInRange


getMoveCost : List Unit -> Float
getMoveCost units =
    List.foldl
        (\unit cost ->
            cost + Game.UnitType.moveCost unit.unitType
        )
        0
        units


analyzeBuildUnits : Core.Model -> List Option
analyzeBuildUnits model =
    let
        cellsWithStructures : List Cell
        cellsWithStructures =
            List.filter
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
                (Game.Grid.toList model.grid)
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
                                { loc = cell.loc, action = BuildUnit unitType, score = 0 }
                            )
                            (List.filter
                                (\unitType ->
                                    Game.UnitType.cost unitType <= stats.gold && stats.units < stats.farms
                                )
                                (Structure.canBuild cell.structure stats.techTree)
                            )
            in
            getUnitOptions stats remainingCells newOptions


scoreOptions : Core.Model -> List Option -> List Option
scoreOptions model options =
    List.map (scoreOption model) options


scoreOption : Core.Model -> Option -> Option
scoreOption model option =
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
    in
    case option.action of
        Move units toLoc ->
            let
                targetCell =
                    Game.Cell.atLoc model.grid toLoc

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

                                                    defBonus =
                                                        targetCell.defBonus
                                                in
                                                -- Don't attack if you're weaker than the enemy
                                                -- Doesn't account for everything, but that's intentional
                                                if attackerThreat < defenderThreat then
                                                    -1000

                                                else
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
                        Attack units toLoc

                    else
                        Move units toLoc
            }

        BuildUnit unitType ->
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

        BuildStructure _ ->
            option

        Research research ->
            if stats.gold <= research.cost || stats.farms == stats.units || analyzer == Passive then
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

                                               else
                                                0
                                                    + (if analyzer == Aggressive then
                                                        200

                                                       else
                                                        0
                                                      )
                                              )
                                }

                            TechTree.BuildWarriors ->
                                { option
                                    | score =
                                        200
                                            + (if isTopHalf then
                                                200

                                               else
                                                0
                                                    + (if analyzer == Defensive then
                                                        200

                                                       else
                                                        0
                                                      )
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

                                               else
                                                0
                                                    + (if isTopHalf then
                                                        500

                                                       else
                                                        0
                                                      )
                                              )
                                }

                            TechTree.BuildRogues ->
                                { option
                                    | score =
                                        200
                                            + (if not isTopHalf then
                                                300

                                               else
                                                0
                                                    + (if analyzer == Defensive then
                                                        300

                                                       else
                                                        0
                                                      )
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

                                               else
                                                0
                                                    + (if analyzer == Default then
                                                        1000

                                                       else
                                                        0
                                                      )
                                              )
                                }

                            TechTree.BuildPriests ->
                                { option
                                    | score =
                                        500
                                            + (if stats.farms > 15 then
                                                500

                                               else
                                                0
                                                    + (if analyzer == Defensive then
                                                        1000

                                                       else
                                                        0
                                                      )
                                              )
                                }

        Upgrade upgradeType ->
            let
                cell =
                    Game.Cell.atLoc model.grid option.loc
            in
            case upgradeType of
                Game.Analysis.RepairDefense ->
                    { option
                        | score =
                            if stats.gold < 1 then
                                -1000

                            else
                                0
                                    + (if stats.units < round (toFloat stats.farms / 2) then
                                        -100

                                       else
                                        0
                                            + (if cell.defBonus <= 0 then
                                                300

                                               else
                                                0
                                                    + (if analyzer /= Aggressive then
                                                        150

                                                       else
                                                        25
                                                      )
                                              )
                                      )
                    }

                Game.Analysis.BuildFarm ->
                    { option
                        | score =
                            if stats.gold < 2 then
                                -1000

                            else
                                1
                    }

                Game.Analysis.BuildTower ->
                    { option
                        | score =
                            if stats.gold < 2 then
                                -1000

                            else
                                1
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
