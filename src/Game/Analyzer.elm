module Game.Analyzer exposing (..)

import Game.Core as Core exposing (PlayerStats)
import Juralen.Analysis exposing (Action(..), Option)
import Juralen.Cell exposing (Cell, Loc)
import Juralen.CellType
import Juralen.Grid
import Juralen.Structure exposing (Structure)
import Juralen.Unit exposing (Unit)
import Juralen.UnitType exposing (UnitType)
import Juralen.AnalyzerMode exposing (AnalyzerMode(..))
import Juralen.TechTree as TechTree exposing (TechLevel(..))



-- 1. Get options
-- 1.1 Find movement options            // analyzeMoves : Core.Model -> List Option
-- 1.2 Find units to build              // analyzeBuildUnits : Core.Model -> List Option
-- 1.3 Find structures to build         // analyzeBuildStructure : Core.Model -> List Option
-- 2. Score options                     // scoreOptions : List Option -> List Option
-- 3. Sort options by score             // sortOptions : List Option -> List Option
-- 4. Filter undesireable options       // filterBadOptions : List Option -> List Option
-- 5. Return Maybe Option (List.head)   // getBestOption : List Option -> Maybe Option


analyze : Core.Model -> List Option
analyze model =
    analyzeBuildUnits model
        ++ analyzeMoves model
        ++ analyzeResearchOptions model
        |> scoreOptions model
        |> List.filter (\option -> option.score > 0)
        |> sortByScore


type alias UnitOption =
    { unitOptions : List Unit
    , cell : Cell
    }

analyzeResearchOptions : Core.Model -> List Option
analyzeResearchOptions model =
    Core.getPlayerTechTree model.players model.activePlayer 
        |> TechTree.nextAvailableTech 
        |> List.map (\tech -> 
            { loc = { x = 0, y = 0 }
            , action = Research tech
            , score = 0
            })

analyzeMoves : Core.Model -> List Option
analyzeMoves model =
    let
        actions : Float
        actions =
            (Core.currentPlayerStats model).actions

        myUnits =
            Juralen.Unit.controlledBy model.units model.activePlayer

        cellsList =
            model.grid |> Juralen.Grid.toList

        cellsWithUnits =
            cellsList |> List.filter (\cell -> List.length (Juralen.Unit.inCell myUnits { x = cell.x, y = cell.y }) > 0)

        unitCombinations : List UnitOption
        unitCombinations =
            toList 
                (List.map 
                    (\cell -> 
                        List.foldl 
                            (\units combinations -> 
                                combinations ++ [ { unitOptions = units, cell = cell } ]
                            ) [] 
                            (combineUnitWith [] [] (Juralen.Unit.inCell myUnits { x = cell.x, y = cell.y }))
                    ) 
                    cellsWithUnits
                )
    in
    toList (List.map (\combination -> getMoveOptions actions { x = combination.cell.x, y = combination.cell.y } cellsList combination.unitOptions []) unitCombinations)


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


getMoveOptions : Float -> Loc -> List Cell -> List Unit -> List Option -> List Option
getMoveOptions actions fromLoc cellsInRange units options =
    let
        toLoc : Loc
        toLoc =
            case List.head cellsInRange of
                Nothing ->
                    { x = -1, y = -1 }

                Just cell ->
                    { x = cell.x, y = cell.y }

        remainingCells =
            List.tail cellsInRange

        distance : Int
        distance =
            Juralen.Cell.getDistance fromLoc toLoc

        moveCost : Float
        moveCost =
            getMoveCost units

        thisActionCost =
            Basics.toFloat distance * moveCost

        newOptions =
            if thisActionCost <= actions && distance /= 0 then
                options ++ [ { loc = fromLoc, action = Move units toLoc, score = 0 } ]

            else
                options
    in
    case remainingCells of
        Nothing ->
            newOptions

        Just cellsList ->
            getMoveOptions actions fromLoc cellsList units newOptions


isInRange : Float -> List Unit -> Loc -> Cell -> Bool
isInRange actions units selectedCell cell =
    List.length units
        > 0
        && selectedCell
        /= { x = cell.x, y = cell.y }
        && Juralen.CellType.isPassable cell.cellType
        && actions
        >= (Basics.toFloat (Juralen.Cell.getDistance selectedCell { x = cell.x, y = cell.y }) * getMoveCost units)


getMoveCost : List Unit -> Float
getMoveCost units =
    List.foldl
        (\unit cost ->
            cost + Juralen.UnitType.moveCost unit.unitType
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
                        /= Nothing
                        && (case cell.controlledBy of
                                Nothing ->
                                    False

                                Just controlledBy ->
                                    controlledBy == model.activePlayer
                           )
                )
                (Juralen.Grid.toList model.grid)
    in
    getUnitOptions (Core.currentPlayerStats model) cellsWithStructures []


getUnitOptions : PlayerStats -> List Cell -> List Option -> List Option
getUnitOptions stats cells options =
    let
        cell =
            case List.head cells of
                Nothing ->
                    Juralen.Cell.empty

                Just thisCell ->
                    thisCell

        remainingCells =
            case List.tail cells of
                Nothing ->
                    []

                Just remainder ->
                    remainder

        newOptions =
            options
                ++ List.map
                    (\unitType ->
                        { loc = { x = cell.x, y = cell.y }, action = BuildUnit unitType, score = 0 }
                    )
                    (List.filter
                        (\unitType ->
                            Juralen.UnitType.cost unitType <= stats.gold && stats.units < stats.farms
                        )
                        (Juralen.Structure.canBuild cell.structure stats.techTree)
                    )
    in
    if List.length remainingCells <= 0 then
        newOptions

    else
        getUnitOptions stats remainingCells newOptions


scoreOptions : Core.Model -> List Option -> List Option
scoreOptions model options =
    List.map (scoreOption model) options


scoreOption : Core.Model -> Option -> Option
scoreOption model option =
    let
        analyzer = Core.playerAnalyzer model.players model.activePlayer

        stats = Core.currentPlayerStats model

        rank = Core.getPlayerRanking (Core.getPlayerRankings model) model.activePlayer 1

        isFirstPlace = rank == 1
        isLastPlace = rank == List.length model.players
        isTopHalf = round ((toFloat rank / toFloat (List.length model.players)) * 100) < 50
        isBottomHalf = round ((toFloat rank / toFloat (List.length model.players)) * 100) >= 50
    in

    case option.action of
        Move units toLoc ->
            let
                targetCell =
                    Juralen.Cell.atLoc (Juralen.Grid.toList model.grid) toLoc

                isOnlyPriests : Bool
                isOnlyPriests = 
                    List.foldl (\unit isPriests -> isPriests && unit.unitType == Juralen.UnitType.Priest) True units

                score =
                    10
                        -- Subtract number of units to move * 2 (fewer units is better)
                        - List.length units * (
                            if analyzer == Juralen.AnalyzerMode.Defensive then 3 else 2
                        )
                        -- Don't move units without moves left
                        - (if List.any (\unit -> unit.movesLeft <= 0) units then
                            1000

                           else
                            0
                          )
                        -- Subtract distance (shorter is better)
                        - Juralen.Cell.getDistance option.loc toLoc * (if analyzer == Juralen.AnalyzerMode.Defensive then 3 else 1)
                        -- Add based on cell type (plains are better than forests)
                        -- Don't move to mountains
                        + (case targetCell.cellType of
                            Juralen.CellType.Plains ->
                                15

                            Juralen.CellType.Mountain ->
                                -1000

                            _ ->
                                0
                          )
                        -- Add if cell has a structure (capturing structures is better)
                        + (if targetCell.structure /= Nothing then
                            case targetCell.controlledBy of
                                Nothing ->
                                    100
                                
                                Just playerId ->
                                    if 
                                        playerId == model.activePlayer 
                                    then
                                        if List.length (Juralen.Unit.inCell model.units { x = targetCell.x, y = targetCell.y }) <= 0 then 120 else 0
                                    else  
                                        if analyzer == Juralen.AnalyzerMode.Aggressive then 125 else 75

                           else
                            if analyzer == Juralen.AnalyzerMode.Aggressive then 100 else 0
                          )
                        -- Is it worth moving there?
                            -- No impact if cell is not controlled.
                            -- Negative if it is controller by active player
                            -- Add if controlled by someone else (capturing enemy territory is better)
                                -- Analyze threat of enemy units before making a decision, attacking is less important than winning
                                -- Include defensive bonus of cell before attacking
                        + (case targetCell.controlledBy of
                            Nothing ->
                                0

                            Just playerId ->
                                if playerId == model.activePlayer then
                                    -100

                                else 
                                    Core.getPlayerScore model playerId
                                    +
                                    if List.length (Juralen.Unit.inCell model.units { x = targetCell.x, y = targetCell.y }) > 0 then
                                        if analyzer == Juralen.AnalyzerMode.Passive -- || isOnlyPriests
                                        then -1000 else

                                        List.foldl (\unit threat -> threat + Juralen.UnitType.threat unit) 0 
                                            (List.map (\unit -> unit.unitType) units)
                                        - 
                                        List.foldl (\unit threat -> threat + Juralen.UnitType.threat unit) 0 
                                            (List.map (\unit -> unit.unitType) (Juralen.Unit.inCell model.units { x = targetCell.x, y = targetCell.y }))
                                        - targetCell.defBonus
                                        

                                    else
                                        if analyzer == Juralen.AnalyzerMode.Aggressive then 100 else 50
                          )
                        -- Should we move everyone in this cell? Do we really need more farms?
                        -- Defending territory is preferable if we can get away with it
                        + (if List.length (Juralen.Unit.inCell model.units option.loc) <= List.length units then
                            if stats.farms == stats.units then
                                10

                            else
                                -1000

                           else
                            0
                          )
                
                isAttack = 
                    case targetCell.controlledBy of
                        Nothing ->
                            False

                        Just playerId ->
                            if playerId == model.activePlayer then False
                            else True
            in
            { option | score = score, action = if isAttack then Attack units toLoc else Move units toLoc }

        BuildUnit unitType ->
            let
                unitsInCell = List.length (Juralen.Unit.inCell model.units option.loc)

                score =
                    -- Build based on priority (higher scores are better)
                    1 
                    + unitTypeScore unitType
                    - round (toFloat (List.length (List.filter (\unit -> unit.unitType == unitType && unit.controlledBy == model.activePlayer) model.units)) / 2)
                    -- Build based on current unit count in cell (lower pop of units is more important)
                    +
                        if unitsInCell <= 0
                        then
                            1000
                        else
                            5 - unitsInCell


            in
            { option | score = score }

        BuildStructure structure ->
            option

        Research research ->
            if stats.gold <= research.cost || stats.farms == stats.units || analyzer == Passive then
                { option | score = -1000 }

            else
                case research.tech of
                    LevelOne tech ->
                        { option | score = 200 }

                    LevelTwo tech ->
                        case tech of
                            TechTree.BuildArchers ->
                                { option | score = 200 + if isBottomHalf then 200 else 0 + if analyzer == Aggressive then 200 else 0 }
                            
                            TechTree.BuildWarriors ->
                                { option | score = 200 + if isTopHalf then 200 else 0 + if analyzer == Defensive then 200 else 0 }

                    LevelThree tech ->
                        case tech of
                            TechTree.BuildKnights ->
                                { option | score = 200 + if analyzer == Aggressive then 1000 else 0 + if isTopHalf then 500 else 0}

                            TechTree.BuildRogues ->
                                { option | score = 200 + if not isTopHalf then 300 else 0 + if analyzer == Defensive then 300 else 0}

                    LevelFour tech ->
                        case tech of
                            TechTree.BuildWizards ->
                                { option | score = 500 + if stats.gold > 25 then 1000 else 0 + if analyzer == Default then 1000 else 0 }

                            TechTree.BuildPriests ->
                                { option | score = 500 + if stats.farms > 15 then 500 else 0 + if analyzer == Defensive then 1000 else 0 }

        _ ->
            option


unitTypeScore : UnitType -> Int
unitTypeScore unitType =
    case unitType of
        Juralen.UnitType.Soldier ->
            1

        Juralen.UnitType.Warrior ->
            2

        Juralen.UnitType.Archer ->
            2

        Juralen.UnitType.Priest ->
            4

        Juralen.UnitType.Rogue ->
            3

        Juralen.UnitType.Wizard ->
            4

        Juralen.UnitType.Knight ->
            3


sortByScore : List Option -> List Option
sortByScore options =
    List.sortBy .score options |> List.reverse
