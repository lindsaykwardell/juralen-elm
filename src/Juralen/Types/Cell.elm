module Juralen.Types.Cell exposing (..)

import Juralen.Types.Loc exposing (Loc)
import Juralen.Types.Structure exposing (Structure, buildCitadel)
import Juralen.Types.Player exposing (Player)

type alias Cell =
    { controlledBy : Maybe Int
    , defBonus : Int
    , passable : Bool
    , structure : Maybe Structure
    , terrain : String
    , x : Int
    , y : Int
    }


generateCell : Loc -> Int -> Cell
generateCell loc roll =
    if roll <= 12 then
        { controlledBy = Nothing
        , defBonus = 3
        , passable = True
        , structure =
            Just
                { buildUnits = [ "Soldier" ]
                , initDefBonus = 3
                , name = "Town"
                }
        , terrain = "Plains"
        , x = loc.x
        , y = loc.y
        }

    else if roll > 12 && roll <= 20 then
        { controlledBy = Nothing
        , defBonus = 0
        , passable = False
        , structure = Nothing
        , terrain = "Mountain"
        , x = loc.x
        , y = loc.y
        }

    else if roll > 20 && roll <= 40 then
        { controlledBy = Nothing
        , defBonus = 1
        , passable = True
        , structure = Nothing
        , terrain = "Forest"
        , x = loc.x
        , y = loc.y
        }

    else
        { controlledBy = Nothing
        , defBonus = 0
        , passable = True
        , structure = Nothing
        , terrain = "Plains"
        , x = loc.x
        , y = loc.y
        }


getStructure : Maybe Structure -> String
getStructure structure =
    case structure of
        Nothing ->
            ""

        Just realStructure ->
            realStructure.name


buildStructure : Cell -> String -> Cell
buildStructure cell structureName =
    { cell | structure = Just buildCitadel, terrain = "Plains", passable = True, defBonus = 7 }


controlledBy : Cell -> Player -> Cell
controlledBy cell player =
    { cell | controlledBy = Just player.id }