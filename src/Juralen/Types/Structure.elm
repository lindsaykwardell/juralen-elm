module Juralen.Types.Structure exposing (..)


type alias Structure =
    { buildUnits : List String
    , initDefBonus : Int
    , name : String
    }


buildCitadel : Structure
buildCitadel =
    { buildUnits = [ "Soldier", "Warrior" ]
    , initDefBonus = 7
    , name = "Citadel"
    }
