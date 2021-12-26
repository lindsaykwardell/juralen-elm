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
        |> Decode.optional "levelOne"
            (Decode.string
                |> Decode.andThen
                    (\tech ->
                        case tech of
                            "buildFarms" ->
                                Decode.succeed (Just BuildFarms)

                            "buildActions" ->
                                Decode.succeed (Just BuildActions)

                            _ ->
                                Decode.fail "Invalid tech tree"
                    )
            )
            Nothing
        |> Decode.optional "levelTwo"
            (Decode.string
                |> Decode.andThen
                    (\tech ->
                        case tech of
                            "buildWarriors" ->
                                Decode.succeed (Just BuildWarriors)

                            "buildArchers" ->
                                Decode.succeed (Just BuildArchers)

                            _ ->
                                Decode.fail "Invalid tech tree"
                    )
            )
            Nothing
        |> Decode.optional "levelThree"
            (Decode.string
                |> Decode.andThen
                    (\tech ->
                        case tech of
                            "buildKnights" ->
                                Decode.succeed (Just BuildKnights)

                            "buildRogues" ->
                                Decode.succeed (Just BuildRogues)

                            _ ->
                                Decode.fail "Invalid tech tree"
                    )
            )
            Nothing
        |> Decode.optional "levelFour"
            (Decode.string
                |> Decode.andThen
                    (\tech ->
                        case tech of
                            "buildWizards" ->
                                Decode.succeed (Just BuildWizards)

                            "buildPriests" ->
                                Decode.succeed (Just BuildPriests)

                            _ ->
                                Decode.fail "Invalid tech tree"
                    )
            )
            Nothing


encoder : TechTree -> Encode.Value
encoder techTree =
    Encode.object
        [ ( "levelOne"
          , case techTree.levelOne of
                Nothing ->
                    Encode.null

                Just levelOne ->
                    case levelOne of
                        BuildFarms ->
                            Encode.string "buildFarms"

                        BuildActions ->
                            Encode.string "buildActions"
          )
        , ( "levelTwo"
          , case techTree.levelTwo of
                Nothing ->
                    Encode.null

                Just levelTwo ->
                    case levelTwo of
                        BuildWarriors ->
                            Encode.string "buildWarriors"

                        BuildArchers ->
                            Encode.string "buildArchers"
          )
        , ( "levelThree"
          , case techTree.levelThree of
                Nothing ->
                    Encode.null

                Just levelThree ->
                    case levelThree of
                        BuildKnights ->
                            Encode.string "buildKnights"

                        BuildRogues ->
                            Encode.string "buildRogues"
          )
        , ( "levelFour"
          , case techTree.levelFour of
                Nothing ->
                    Encode.null

                Just levelFour ->
                    case levelFour of
                        BuildWizards ->
                            Encode.string "buildWizards"

                        BuildPriests ->
                            Encode.string "buildPriests"
          )
        ]


type
    TechLevel
    -- = LevelOne LevelOne
    = LevelTwo LevelTwo
    | LevelThree LevelThree
    | LevelFour LevelFour


type LevelOne
    = BuildFarms
    | BuildActions


type LevelTwo
    = BuildWarriors
    | BuildArchers


type LevelThree
    = BuildKnights
    | BuildRogues


type LevelFour
    = BuildWizards
    | BuildPriests


type alias TechDescription =
    { name : String
    , description : String
    , cost : Int
    , tech : TechLevel
    }


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
