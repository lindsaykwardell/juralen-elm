module Game.TechTree exposing (..)


type alias TechTree =
    { levelOne : Maybe LevelOne
    , levelTwo : Maybe LevelTwo
    , levelThree : Maybe LevelThree
    , levelFour : Maybe LevelFour
    }


type TechLevel
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
