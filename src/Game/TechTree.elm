module Game.TechTree exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode


type alias TechTree =
    { levelOne : Maybe LevelOne
    , levelTwo : Maybe LevelTwo
    , levelThree : Maybe LevelThree
    , levelFour : Maybe LevelFour
    }


decoder : Decoder TechTree
decoder =
    Decode.succeed TechTree
        |> Decode.required "levelOne" (Decode.nullable levelOneDecoder)
        |> Decode.required "levelTwo" (Decode.nullable levelTwoDecoder)
        |> Decode.required "levelThree" (Decode.nullable levelThreeDecoder)
        |> Decode.required "levelFour" (Decode.nullable levelFourDecoder)


encoder : TechTree -> Encode.Value
encoder techTree =
    Encode.object
        [ ( "levelOne"
          , levelOneEncoder techTree.levelOne
          )
        , ( "levelTwo"
          , levelTwoEncoder techTree.levelTwo
          )
        , ( "levelThree"
          , levelThreeEncoder techTree.levelThree
          )
        , ( "levelFour"
          , levelFourEncoder techTree.levelFour
          )
        ]


type
    TechLevel
    -- = LevelOne LevelOne
    = LevelTwo LevelTwo
    | LevelThree LevelThree
    | LevelFour LevelFour


techLevelDecoder : Decoder TechLevel
techLevelDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                if String.contains "two" str then
                    case str |> Encode.string |> Encode.encode 0 |> Decode.decodeString levelTwoDecoder of
                        Ok levelTwo ->
                            Decode.succeed (LevelTwo levelTwo)

                        Err _ ->
                            Decode.fail "LevelTwo"

                else if String.contains "three" str then
                    case str |> Encode.string |> Encode.encode 0 |> Decode.decodeString levelThreeDecoder of
                        Ok levelThree ->
                            Decode.succeed (LevelThree levelThree)

                        Err _ ->
                            Decode.fail "LevelThree"

                else if String.contains "four" str then
                    case str |> Encode.string |> Encode.encode 0 |> Decode.decodeString levelFourDecoder of
                        Ok levelFour ->
                            Decode.succeed (LevelFour levelFour)

                        Err _ ->
                            Decode.fail "LevelFour"

                else
                    Decode.fail "TechLevel"
            )


techLevelEncoder : TechLevel -> Encode.Value
techLevelEncoder techLevel =
    case techLevel of
        -- LevelOne levelOne ->
        --      Encode.encodeString levelOneEncoder levelOne
        LevelTwo levelTwo ->
            levelTwoEncoder (Just levelTwo)

        LevelThree levelThree ->
            levelThreeEncoder (Just levelThree)

        LevelFour levelFour ->
            levelFourEncoder (Just levelFour)


type LevelOne
    = BuildFarms
    | BuildActions


levelOneDecoder : Decoder LevelOne
levelOneDecoder =
    Decode.string
        |> Decode.andThen
            (\tech ->
                case tech of
                    "one:buildFarms" ->
                        Decode.succeed BuildFarms

                    "one:buildActions" ->
                        Decode.succeed BuildActions

                    _ ->
                        Decode.fail "Invalid tech tree"
            )


levelOneEncoder : Maybe LevelOne -> Encode.Value
levelOneEncoder levelOne =
    case levelOne of
        Nothing ->
            Encode.null

        Just l1 ->
            case l1 of
                BuildFarms ->
                    Encode.string "one:buildFarms"

                BuildActions ->
                    Encode.string "one:buildActions"


type LevelTwo
    = BuildWarriors
    | BuildArchers


levelTwoDecoder : Decoder LevelTwo
levelTwoDecoder =
    Decode.string
        |> Decode.andThen
            (\tech ->
                case tech of
                    "two:buildWarriors" ->
                        Decode.succeed BuildWarriors

                    "two:buildArchers" ->
                        Decode.succeed BuildArchers

                    _ ->
                        Decode.fail "Invalid tech tree"
            )


levelTwoEncoder : Maybe LevelTwo -> Encode.Value
levelTwoEncoder levelTwo =
    case levelTwo of
        Nothing ->
            Encode.null

        Just l2 ->
            case l2 of
                BuildWarriors ->
                    Encode.string "two:buildWarriors"

                BuildArchers ->
                    Encode.string "two:buildArchers"


type LevelThree
    = BuildKnights
    | BuildRogues


levelThreeDecoder : Decoder LevelThree
levelThreeDecoder =
    Decode.string
        |> Decode.andThen
            (\tech ->
                case tech of
                    "three:buildKnights" ->
                        Decode.succeed BuildKnights

                    "three:buildRogues" ->
                        Decode.succeed BuildRogues

                    _ ->
                        Decode.fail "Invalid tech tree"
            )


levelThreeEncoder : Maybe LevelThree -> Encode.Value
levelThreeEncoder levelThree =
    case levelThree of
        Nothing ->
            Encode.null

        Just l3 ->
            case l3 of
                BuildKnights ->
                    Encode.string "three:buildKnights"

                BuildRogues ->
                    Encode.string "three:buildRogues"


type LevelFour
    = BuildWizards
    | BuildPriests


levelFourDecoder : Decoder LevelFour
levelFourDecoder =
    Decode.string
        |> Decode.andThen
            (\tech ->
                case tech of
                    "four:buildWizards" ->
                        Decode.succeed BuildWizards

                    "four:buildPriests" ->
                        Decode.succeed BuildPriests

                    _ ->
                        Decode.fail "Invalid tech tree"
            )


levelFourEncoder : Maybe LevelFour -> Encode.Value
levelFourEncoder levelFour =
    case levelFour of
        Nothing ->
            Encode.null

        Just l4 ->
            case l4 of
                BuildWizards ->
                    Encode.string "four:buildWizards"

                BuildPriests ->
                    Encode.string "four:buildPriests"


type alias TechDescription =
    { name : String
    , description : String
    , cost : Int
    , tech : TechLevel
    }


techDescriptionDecoder : Decoder TechDescription
techDescriptionDecoder =
    Decode.succeed TechDescription
        |> Decode.required "name" Decode.string
        |> Decode.required "description" Decode.string
        |> Decode.required "cost" Decode.int
        |> Decode.required "tech" techLevelDecoder


techDescriptionEncoder : TechDescription -> Encode.Value
techDescriptionEncoder desc =
    Encode.object
        [ ( "name"
          , Encode.string desc.name
          )
        , ( "description"
          , Encode.string desc.description
          )
        , ( "cost"
          , Encode.int desc.cost
          )
        , ( "tech"
          , techLevelEncoder desc.tech
          )
        ]


empty : TechTree
empty =
    { levelOne = Nothing
    , levelTwo = Nothing
    , levelThree = Nothing
    , levelFour = Nothing
    }


nextAvailableTech : TechTree -> List TechDescription
nextAvailableTech techTree =
    -- if techTree.levelOne == Nothing then
    --     [ techDescription (LevelOne BuildFarms), techDescription (LevelOne BuildActions) ]
    if techTree.levelTwo == Nothing then
        [ techDescription (LevelTwo BuildWarriors), techDescription (LevelTwo BuildArchers) ]

    else if techTree.levelThree == Nothing then
        [ techDescription (LevelThree BuildKnights), techDescription (LevelThree BuildRogues) ]

    else if techTree.levelFour == Nothing then
        [ techDescription (LevelFour BuildWizards), techDescription (LevelFour BuildPriests) ]

    else
        []


research : TechTree -> TechLevel -> TechTree
research techTree level =
    case level of
        -- LevelOne tech ->
        --     { techTree | levelOne = Just tech }
        LevelTwo tech ->
            { techTree | levelTwo = Just tech }

        LevelThree tech ->
            { techTree | levelThree = Just tech }

        LevelFour tech ->
            { techTree | levelFour = Just tech }


techCost : TechLevel -> Int
techCost level =
    case level of
        -- LevelOne _ ->
        --     3
        LevelTwo _ ->
            5

        LevelThree _ ->
            8

        LevelFour _ ->
            13


techDescription : TechLevel -> TechDescription
techDescription level =
    case level of
        -- LevelOne levelOne ->
        --     case levelOne of
        --         BuildFarms ->
        --             { name = "Build Farms", description = "Build more farms in your towns and citadel.", cost = techCost level, tech = level }
        --         BuildActions ->
        --             { name = "Build Towers", description = "Build towers to generate more actions.", cost = techCost level, tech = level }
        LevelTwo levelTwo ->
            case levelTwo of
                BuildWarriors ->
                    { name = "Build Warriors", description = "Advanced unit that deals more damage.", cost = techCost level, tech = level }

                BuildArchers ->
                    { name = "Build Archers", description = "Advanced unit that attacks at range.", cost = techCost level, tech = level }

        LevelThree levelThree ->
            case levelThree of
                BuildKnights ->
                    { name = "Build Knights", description = "Mobile unit that deals high damage.", cost = techCost level, tech = level }

                BuildRogues ->
                    { name = "Build Rogues", description = "Advanced unit with high damage, but is weaker.", cost = techCost level, tech = level }

        LevelFour levelFour ->
            case levelFour of
                BuildWizards ->
                    { name = "Build Wizards", description = "Advanced unit with lower movement cost.", cost = techCost level, tech = level }

                BuildPriests ->
                    { name = "Build Priests", description = "Advanced unit that heals other units over time.", cost = techCost level, tech = level }
