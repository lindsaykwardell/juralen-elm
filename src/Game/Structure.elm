module Game.Structure exposing (..)

import Game.TechTree exposing (TechTree)
import Game.UnitType exposing (UnitType(..))
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


type Structure
    = Town
    | Citadel
    | None


decoder : Decoder Structure
decoder =
    Decode.string
        |> Decode.andThen
            (\structure ->
                case structure of
                    "town" ->
                        Decode.succeed Town

                    "citadel" ->
                        Decode.succeed Citadel

                    "none" ->
                        Decode.succeed None

                    _ ->
                        Decode.fail "Invalid structure"
            )


encoder : Structure -> Encode.Value
encoder structure =
    case structure of
        Town ->
            Encode.string "town"

        Citadel ->
            Encode.string "citadel"

        None ->
            Encode.string "none"


initDef : Structure -> Int
initDef structure =
    case structure of
        Town ->
            3

        Citadel ->
            5

        _ ->
            0


canBuild : Structure -> TechTree -> List UnitType
canBuild structure techTree =
    case structure of
        None ->
            []

        Town ->
            [ Soldier ]

        Citadel ->
            Game.TechTree.researchedUnits techTree


toString : Structure -> String
toString structure =
    case structure of
        Town ->
            "Town"

        Citadel ->
            "Citadel"

        None ->
            ""


getCellClass : Structure -> String
getCellClass structure =
    case structure of
        Town ->
            "town"

        Citadel ->
            "citadel"

        None ->
            ""


maxUpgradeCount : Structure -> Int
maxUpgradeCount structure =
    case structure of
        Citadel ->
            6

        Town ->
            4

        None ->
            0
