module Juralen.Types.Unit exposing (..)


type alias Unit =
    { id : String
    , name : String
    , cost : Int
    , move : Float
    , movesLeft : Int
    , maxMoves : Int
    , attack : Int
    , health : Int
    , maxHealth : Int
    , range : Int
    , description : String
    , controlledBy : String
    , x : Int
    , y : Int
    }