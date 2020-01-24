module Juralen.Init.Init exposing (..)

import Random


randomTileNumber : Random.Generator Int
randomTileNumber =
    Random.int 0 101
