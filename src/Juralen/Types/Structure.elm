module Juralen.Types.Structure exposing (..)

import Juralen.Types.UnitType exposing (UnitType)


type alias Structure =
    { buildUnits : List UnitType
    , initDefBonus : Int
    , name : String
    }


buildCitadel : Structure
buildCitadel =
    { buildUnits = [ Juralen.Types.UnitType.Soldier, Juralen.Types.UnitType.Warrior ]
    , initDefBonus = 7
    , name = "Citadel"
    }
