module Juralen.Resources exposing (..)


type alias Resources =
    { actions : Float
    , gold : Int
    }

empty : Resources
empty =
    { actions = 0, gold = 0}

spend : Resources -> Int -> Resources
spend resources cost =
    { resources | gold = resources.gold - cost}
