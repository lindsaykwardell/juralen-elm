module Game.Cell exposing (Cell, buildStructure, decoder, empty, encoder, generate, getBorderingPlayer, getColorClass, hasStructure, updateControl)

import Game.CellType exposing (CellType)
import Game.Loc as Loc exposing (Loc)
import Game.Player exposing (Player)
import Game.PlayerColor
import Game.Structure as Structure exposing (Structure)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


type alias Cell =
    { cellType : CellType
    , controlledBy : Maybe Int
    , defBonus : Int
    , structure : Structure
    , farms : Int
    , towers : Int
    , loc : Loc
    }


decoder : Decoder Cell
decoder =
    Decode.succeed Cell
        |> Decode.required "cellType" Game.CellType.decoder
        |> Decode.required "controlledBy" (Decode.nullable Decode.int)
        |> Decode.required "defBonus" Decode.int
        |> Decode.required "structure" Structure.decoder
        |> Decode.required "farms" Decode.int
        |> Decode.required "towers" Decode.int
        |> Decode.required "loc" Loc.decoder


encoder : Cell -> Encode.Value
encoder cell =
    Encode.object
        [ ( "cellType", Game.CellType.encoder cell.cellType )
        , ( "controlledBy"
          , case cell.controlledBy of
                Nothing ->
                    Encode.null

                Just player ->
                    Encode.int player
          )
        , ( "defBonus", Encode.int cell.defBonus )
        , ( "structure", Structure.encoder cell.structure )
        , ( "farms", Encode.int cell.farms )
        , ( "towers", Encode.int cell.towers )
        , ( "loc", Loc.encoder cell.loc )
        ]


generate : Loc -> Int -> Cell
generate loc roll =
    if roll <= 12 then
        { cellType = Game.CellType.Plains
        , controlledBy = Nothing
        , defBonus = 3
        , structure = Structure.Town
        , farms = 0
        , towers = 0
        , loc = loc
        }

    else if roll > 12 && roll <= 20 then
        { cellType = Game.CellType.Mountain
        , controlledBy = Nothing
        , defBonus = 0
        , structure = Structure.None
        , farms = 0
        , towers = 0
        , loc = loc
        }

    else if roll > 20 && roll <= 40 then
        { cellType = Game.CellType.Forest
        , controlledBy = Nothing
        , defBonus = 1
        , structure = Structure.None
        , farms = 0
        , towers = 0
        , loc = loc
        }

    else
        { cellType = Game.CellType.Plains
        , controlledBy = Nothing
        , defBonus = 0
        , structure = Structure.None
        , farms = 0
        , towers = 0
        , loc = loc
        }


empty : Cell
empty =
    { cellType = Game.CellType.Plains
    , controlledBy = Nothing
    , defBonus = -1
    , structure = Structure.None
    , farms = 0
    , towers = 0
    , loc = Loc.at -1 -1
    }


hasStructure : Cell -> Bool
hasStructure cell =
    cell.structure /= Structure.None


buildStructure : Cell -> Structure -> Cell
buildStructure cell structure =
    { cell | structure = structure, cellType = Game.CellType.Plains, defBonus = 5 }


updateControl : Cell -> Int -> Cell
updateControl cell playerId =
    if cell.cellType == Game.CellType.Plains then
        { cell | controlledBy = Just playerId }

    else
        cell


getColorClass : Cell -> List Player -> String
getColorClass cell players =
    case cell.controlledBy of
        Nothing ->
            Game.CellType.getColorClass cell.cellType

        Just playerId ->
            Game.Player.get players playerId
                |> .color
                |> Game.PlayerColor.toClass


getBorderingPlayer : List Cell -> List (Maybe Int) -> List (Maybe Int)
getBorderingPlayer cells players =
    case cells of
        [] ->
            players

        cell :: remainingCells ->
            if cell.cellType == Game.CellType.Mountain then
                getBorderingPlayer remainingCells players

            else
                getBorderingPlayer remainingCells (players ++ [ cell.controlledBy ])
