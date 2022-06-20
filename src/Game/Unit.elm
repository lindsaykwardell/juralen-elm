module Game.Unit exposing (Unit, buildUnit, controlledBy, decoder, empty, encoder, fromId, inCell, isDead, isSelected, takeDamage)

import Game.Level as Level exposing (Level)
import Game.Loc as Loc exposing (Loc)
import Game.UnitType exposing (UnitType)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


type alias Unit =
    { id : Int
    , unitType : UnitType
    , movesLeft : Int
    , maxMoves : Int
    , attack : Int
    , health : Int
    , maxHealth : Int
    , range : Int
    , controlledBy : Int
    , loc : Loc
    , level : Level
    }


decoder : Decoder Unit
decoder =
    Decode.succeed Unit
        |> Decode.required "id" Decode.int
        |> Decode.required "unitType" Game.UnitType.decoder
        |> Decode.required "movesLeft" Decode.int
        |> Decode.required "maxMoves" Decode.int
        |> Decode.required "attack" Decode.int
        |> Decode.required "health" Decode.int
        |> Decode.required "maxHealth" Decode.int
        |> Decode.required "range" Decode.int
        |> Decode.required "controlledBy" Decode.int
        |> Decode.required "loc" Loc.decoder
        |> Decode.required "level" Level.decoder


encoder : Unit -> Encode.Value
encoder unit =
    Encode.object
        [ ( "id", Encode.int unit.id )
        , ( "unitType", Game.UnitType.encoder unit.unitType )
        , ( "movesLeft", Encode.int unit.movesLeft )
        , ( "maxMoves", Encode.int unit.maxMoves )
        , ( "attack", Encode.int unit.attack )
        , ( "health", Encode.int unit.health )
        , ( "maxHealth", Encode.int unit.maxHealth )
        , ( "range", Encode.int unit.range )
        , ( "controlledBy", Encode.int unit.controlledBy )
        , ( "loc", Loc.encoder unit.loc )
        , ( "level", Level.encoder unit.level )
        ]


buildUnit : UnitType -> Int -> Loc -> Int -> Unit
buildUnit unitType playerId loc id =
    let
        initialValues =
            Game.UnitType.initialValues unitType
    in
    { id = id
    , unitType = unitType
    , movesLeft = initialValues.movesLeft
    , maxMoves = initialValues.movesLeft
    , attack = initialValues.attack
    , health = initialValues.health
    , maxHealth = initialValues.health
    , range = initialValues.range
    , controlledBy = playerId
    , loc = loc
    , level = Level.at 1
    }


inCell : List Unit -> Loc -> List Unit
inCell units loc =
    List.filter (\unit -> unit.loc == loc) units


fromId : List Unit -> Int -> Unit
fromId units id =
    case List.head (List.filter (\unit -> unit.id == id) units) of
        Nothing ->
            empty

        Just unit ->
            unit


isSelected : List Int -> Int -> Bool
isSelected selectedUnits unitId =
    case List.head (List.filter (\id -> id == unitId) selectedUnits) of
        Nothing ->
            False

        _ ->
            True


controlledBy : List Unit -> Int -> List Unit
controlledBy units playerId =
    List.filter (\unit -> unit.controlledBy == playerId) units


empty : Unit
empty =
    { id = -1
    , unitType = Game.UnitType.Soldier
    , movesLeft = -1
    , maxMoves = -1
    , attack = 0
    , health = -1
    , maxHealth = -1
    , range = -1
    , controlledBy = -1
    , loc = Loc.at -1 -1
    , level = Level.at -1
    }


takeDamage : Unit -> Int -> Unit
takeDamage unit damage =
    { unit | health = unit.health - damage }


isDead : Unit -> Bool
isDead unit =
    unit.health <= 0
