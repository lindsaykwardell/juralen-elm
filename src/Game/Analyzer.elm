module Game.Analyzer exposing (..)

import Game.Core as Core
import Juralen.Cell exposing (Cell, Loc)
import Juralen.CellType
import Juralen.Grid
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
    analyzeBuildUnits model ++ analyzeMoves model


type alias UnitOption =
    { unitOptions : List Unit
    , cell : Cell
    }


analyzeMoves : Core.Model -> List Option
analyzeMoves model =
    let
        actions : Float
        actions =
            (Core.currentPlayerStats model).actions

        myUnits = Juralen.Unit.controlledBy model.units model.activePlayer

        cellsList = model.grid |> Juralen.Grid.toList

        cellsWithUnits =
            cellsList |> List.filter (\cell -> List.length (Juralen.Unit.inCell myUnits { x = cell.x, y = cell.y }) > 0)

        unitCombinations : List UnitOption
        unitCombinations =
            toList (List.map (\cell -> List.foldl (\units combinations -> combinations ++ [ { unitOptions = units, cell = cell } ]) [] (combineUnitWith [] [] (Juralen.Unit.inCell myUnits { x = cell.x, y = cell.y }))) cellsWithUnits)
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
            unitCombinations ++ [ newCombination ]
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
                options ++ [ { loc = toLoc, action = Move units toLoc, score = 0 } ]

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
            List.filter (\cell -> cell.structure /= Nothing && case cell.controlledBy of
                  Nothing ->
                    False
                    
                  Just controlledBy ->
                      controlledBy == model.activePlayer) (Juralen.Grid.toList model.grid)
    in
    getUnitOptions cellsWithStructures []


getUnitOptions : List Cell -> List Option -> List Option
getUnitOptions cells options =
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
            options ++ List.map (\unitType -> { loc = { x = cell.x, y = cell.y }, action = BuildUnit unitType, score = 0 }) (Juralen.Structure.canBuild cell.structure)
    in
    if List.length remainingCells <= 0 then
        newOptions

    else
        getUnitOptions remainingCells newOptions
